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


import MessageJson
import RequestsHandler
import WebAppState
import State (RuntimeSystem)


runWebApp :: RuntimeSystem -> IO ()
runWebApp system = do
  state <- Concurrent.newMVar WAState {
   system = system,
   clients = []
   }
  Warp.run 3000 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state)
    httpApp

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"



nextId :: WAClients -> WAClientId
nextId = Maybe.maybe 0 ((+) 1) . Safe.maximumMay . List.map fst

connectClient :: WS.Connection -> Concurrent.MVar WAState -> IO WAClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let clientId = nextId (clients state)
  return (
    WAState{
        system = system state,
        clients = (clientId, conn) : (clients state)
    }, clientId)

withoutClient :: WAClientId -> WAClients -> WAClients
withoutClient clientId = List.filter ((/=) clientId . fst)

connectionForClientId :: WAClientId -> WAClients -> Maybe WS.Connection
connectionForClientId clientId clients = snd <$> List.find ((==) clientId . fst) clients

disconnectClient :: WAClientId -> Concurrent.MVar WAState -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
  return WAState {
    system = system state,
    clients = withoutClient clientId (clients state)
  }

listen :: WS.Connection -> WAClientId -> Concurrent.MVar WAState -> IO ()
listen conn clientId stateRef = Monad.forever $ do
  WS.receiveData conn >>= handleMessage clientId stateRef

sendErrorToClient :: WAClientId -> WAClients -> String -> IO ()
sendErrorToClient clientId clients err = case connectionForClientId clientId clients of
                                        Just c -> WS.sendTextData c (Text.pack err)
                                        Nothing -> print $ "clientId not found " ++ show clientId -- nothing else to do.

handleMessage :: WAClientId -> Concurrent.MVar WAState -> Text.Text -> IO ()
handleMessage clientId stateRef json = do
  state <- Concurrent.readMVar stateRef
  case eitherDecode (encodeUtf8 (fromStrict json)) of
        Right r -> handleRequest clientId state r
        Left e -> sendErrorToClient clientId (clients state) e
  return ()
  -- let otherClients = withoutClient clientId clients
  -- Monad.forM_ otherClients $ \(_, conn) ->
    -- WS.sendTextData conn msg

wsApp :: Concurrent.MVar WAState -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn clientId stateRef)
    (disconnectClient clientId stateRef)