{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebApp (runWebApp) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as EX
import qualified Control.Monad as Monad
import qualified Data.Map.Strict as M
import qualified Data.Maybe as B
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import qualified Safe
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

import WebMessages
import WebRequestsHandler
import WebClients
import LogMessages
import Errors
import SystemMessages
import WebAppState



runWebApp :: SystemChan -> LogChan -> IO ()
runWebApp sc lc = do
  state <- Concurrent.newMVar WAState {
   systemChan = sc,
   logChan = lc,
   clients = M.empty
   }
  Warp.run 3000 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state)
    httpApp

wsApp :: Concurrent.MVar WAState -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  cid <- connectClient conn stateRef
  WS.forkPingThread conn 30
  EX.finally
    (listen conn cid stateRef)
    (disconnectClient cid stateRef)

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

connectClient :: WS.Connection -> Concurrent.MVar WAState -> IO WAClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let cid = nextId (clients state)
  print $ "client " ++ show cid ++ " connecting"
  let c = WAClient { clientId = cid, connection = conn }
  return (stateWithClient state c, cid)


disconnectClient :: WAClientId -> Concurrent.MVar WAState -> IO ()
disconnectClient cid stateRef = do
  print $ "client " ++ show cid ++ " disconnecting"
  Concurrent.modifyMVar_ stateRef $ \state -> do
    propagateDisconnection (systemChan state) (clients state) cid
    return $ stateWithoutClient state cid

listen :: WS.Connection -> WAClientId -> Concurrent.MVar WAState -> IO ()
listen conn cid stateRef = Monad.forever receive
  where receive = WS.receiveData conn >>= handleMessage cid stateRef

handleMessage :: WAClientId -> Concurrent.MVar WAState -> B.ByteString -> IO ()
handleMessage cid stateRef jsn = do
  print $ "got request " ++ show jsn ++ " from " ++ show cid
  state <- Concurrent.readMVar stateRef
  case eitherDecode jsn of
        Right r -> handleWebRequest cid state r
        Left e -> sendErrorToClient cid (clients state) (mkError (e ++ " when decoding " ++ show jsn))
  return ()


propagateDisconnection :: SystemChan -> WAClientById -> WAClientId-> IO ()
propagateDisconnection sys cs cid = case M.lookup cid cs of
                                        Just c -> atomically $ writeTChan sys (SMRequest $ SRDisconnect c)
                                        Nothing -> print $ "disconnecting clientId not found " ++ show cid -- nothing else to do.

sendErrorToClient :: WAClientId -> WAClientById -> Error -> IO ()
sendErrorToClient cid cs err = case M.lookup cid cs of
                                        Just c -> sendToClient c (WEError err)
                                        Nothing -> print $ "clientId not found " ++ show cid -- nothing else to do.

stateWithClient :: WAState ->  WAClient -> WAState
stateWithClient state c = WAState{
                     systemChan = systemChan state,
                     logChan = logChan state,
                     clients = M.insert (clientId c) c (clients state)
                 }

stateWithoutClient :: WAState ->  WAClientId -> WAState
stateWithoutClient state cid = WAState {
                    systemChan = systemChan state,
                    logChan = logChan state,
                    clients = M.delete cid (clients state)
                  }

nextId :: WAClientById -> WAClientId
nextId = B.maybe (WAClientId 0) successive . Safe.maximumMay . M.keys
    where successive (WAClientId n) = WAClientId (n + 1)
