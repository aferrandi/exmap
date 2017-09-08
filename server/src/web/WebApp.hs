{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebApp (runWebApp) where

import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import Control.Monad.STM
import qualified Data.List                      as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as Text
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe
import Data.Aeson
import qualified Data.ByteString.Lazy as B


import WebMessages
import WebMessagesJson
import WebRequestsHandler
import WebClients
import SystemMessages
import LogMessages
import Errors


runWebApp :: SystemChan -> LogChan -> IO ()
runWebApp systemChan logChan = do
  state <- Concurrent.newMVar WAState {
   systemChan = systemChan,
   logChan = logChan,
   clients = M.empty
   }
  Warp.run 3000 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state)
    httpApp

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

nextId :: WAClientById -> WAClientId
nextId = Maybe.maybe (WAClientId 0) succ . Safe.maximumMay . M.keys
    where succ (WAClientId n) = WAClientId (n + 1)

connectClient :: WS.Connection -> Concurrent.MVar WAState -> IO WAClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let cid = nextId (clients state)
  let c = WAClient { clientId = cid, connection = conn }
  return (
    WAState{
        systemChan = systemChan state,
        logChan = logChan state,
        clients = M.insert cid c (clients state)
    }, cid)


disconnectClient :: WAClientId -> Concurrent.MVar WAState -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
  return WAState {
    systemChan = systemChan state,
    logChan = logChan state,
    clients = M.delete clientId (clients state)
  }

listen :: WS.Connection -> WAClientId -> Concurrent.MVar WAState -> IO ()
listen conn cid stateRef = Monad.forever $ do
  WS.receiveData conn >>= handleMessage cid stateRef

sendErrorToClient :: WAClientId -> WAClientById -> Error -> IO ()
sendErrorToClient clientId clients err = case M.lookup  clientId clients of
                                        Just c -> sendToClient c (WEError err)
                                        Nothing -> print $ "clientId not found " ++ show clientId -- nothing else to do.

handleMessage :: WAClientId -> Concurrent.MVar WAState -> B.ByteString -> IO ()
handleMessage cid stateRef json = do
  state <- Concurrent.readMVar stateRef
  case eitherDecode json of
        Right r -> atomically $ handleWebRequest cid state r
        Left e -> sendErrorToClient cid (clients state) (mkError e)
  return ()

wsApp :: Concurrent.MVar WAState -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  cid <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn cid stateRef)
    (disconnectClient cid stateRef)