{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebApp (runWebApp) where

import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as Text
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe
import Data.Aeson
import Data.Text.Lazy.Encoding
import Data.Text.Lazy (fromStrict)


import WebMessageJson
import WebRequestsHandler
import WebAppState
import SystemState (SystemChan)


runWebApp :: SystemChan -> IO ()
runWebApp systemChan = do
  state <- Concurrent.newMVar WAState {
   systemChan = systemChan,
   clients = []
   }
  Warp.run 3000 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state)
    httpApp

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"



nextId :: WAClients -> WAClientId
nextId = Maybe.maybe 0 ((+) 1) . Safe.maximumMay . List.map clientId

connectClient :: WS.Connection -> Concurrent.MVar WAState -> IO WAClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let cid = nextId (clients state)
  return (
    WAState{
        systemChan = systemChan state,
        clients = WAClient { clientId = cid, connection = conn } : clients state
    }, cid)

withoutClient :: WAClientId -> WAClients -> WAClients
withoutClient cid = List.filter ((/=)  cid . clientId)

connectionForClientId :: WAClientId -> WAClients -> Maybe WS.Connection
connectionForClientId cid clients = connection <$> List.find ((==) cid . clientId) clients

disconnectClient :: WAClientId -> Concurrent.MVar WAState -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
  return WAState {
    systemChan = systemChan state,
    clients = withoutClient clientId (clients state)
  }

listen :: WS.Connection -> WAClientId -> Concurrent.MVar WAState -> IO ()
listen conn cid stateRef = Monad.forever $ do
  WS.receiveData conn >>= handleMessage cid stateRef

sendErrorToClient :: WAClientId -> WAClients -> String -> IO ()
sendErrorToClient clientId clients err = case connectionForClientId clientId clients of
                                        Just c -> WS.sendTextData c (Text.pack err)
                                        Nothing -> print $ "clientId not found " ++ show clientId -- nothing else to do.

handleMessage :: WAClientId -> Concurrent.MVar WAState -> Text.Text -> IO ()
handleMessage cid stateRef json = do
  state <- Concurrent.readMVar stateRef
  case eitherDecode (encodeUtf8 (fromStrict json)) of
        Right r -> handleWebRequest cid state r
        Left e -> sendErrorToClient cid (clients state) e
  return ()
  -- let otherClients = withoutClient clientId clients
  -- Monad.forM_ otherClients $ \(_, conn) ->
    -- WS.sendTextData conn msg

wsApp :: Concurrent.MVar WAState -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  cid <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn cid stateRef)
    (disconnectClient cid stateRef)