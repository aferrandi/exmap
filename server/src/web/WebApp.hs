{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebApp (runWebApp) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as EX
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Maybe as B
import qualified Data.ByteString.Lazy.Search as BS
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import qualified Safe
import Data.Aeson
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

import WebMessages
import WebRequestsHandler
import WebClients
import LogMessages
import Errors
import SystemMessages
import WebAppState

runWebApp :: SystemChan -> LogChan -> BL.ByteString -> IO ()
runWebApp sc lc idx = do
  state <- Concurrent.newMVar WAState {
   systemChan = sc,
   logChan = lc,
   clients = M.empty
   }
  Warp.run 3000 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state)
    (httpApp idx)

wsApp :: Concurrent.MVar WAState -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  cid <- connectClient conn stateRef
  WS.forkPingThread conn 30
  EX.finally
    (listen conn cid stateRef)
    (disconnectClient cid stateRef)

httpApp :: BL.ByteString -> Wai.Application
httpApp idx request respond =  respond  $ case Wai.rawPathInfo request of
    "/"     -> httpServeIndex idx request
    _       -> httpNotFound

httpServeIndex :: BL.ByteString -> Wai.Request -> Wai.Response
httpServeIndex idx req = Wai.responseLBS
                           Http.status200
                           [("Content-Type", "text/html")]
                           content
    where content = case Wai.requestHeaderHost req of
                        Just host -> fixWSUrl idx (BL.fromStrict host)
                        Nothing -> idx

fixWSUrl :: BL.ByteString -> BL.ByteString -> BL.ByteString
fixWSUrl idx host = BS.replace "localhost:3000" host idx

httpNotFound :: Wai.Response
httpNotFound =  Wai.responseLBS Http.status400 [] "Not a websocket request"

connectClient :: WS.Connection -> Concurrent.MVar WAState -> IO WAClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let cid = nextId (clients state)
  print $ "client " ++ show cid ++ " connecting"
  let c = WAClient { clientId = cid, connection = conn }
  return (stateWithClient state c, cid)


disconnectClient :: WAClientId -> Concurrent.MVar WAState -> IO ()
disconnectClient cid stateRef =
  Concurrent.modifyMVar_ stateRef $ \state -> do
    atomically $ do
        webLogDbg state $ "client " ++ show cid ++ " disconnecting"
        propagateDisconnection state (clients state) cid
    return $ stateWithoutClient state cid

listen :: WS.Connection -> WAClientId -> Concurrent.MVar WAState -> IO ()
listen conn cid stateRef = Monad.forever receive
  where receive = WS.receiveData conn >>= handleMessage cid stateRef

handleMessage :: WAClientId -> Concurrent.MVar WAState -> BL.ByteString -> IO ()
handleMessage cid stateRef jsn = do
    state <- Concurrent.readMVar stateRef
    atomically $ webLogDbg state $ "got request " ++ show jsn ++ " from " ++ show cid
    case eitherDecode jsn of
        Right r -> atomically $ handleWebRequest cid state r
        Left e -> sendErrorToClient state cid (mkError (e ++ " when decoding " ++ show jsn))
    return ()


propagateDisconnection :: WAState -> WAClientById -> WAClientId-> STM ()
propagateDisconnection state cs cid = case M.lookup cid cs of
                                        Just c -> writeTChan (systemChan state) (SMRequest $ SRDisconnect c)
                                        Nothing -> webLogErr state (mkError $ "disconnecting clientId not found " ++ show cid) -- nothing else to do.

sendErrorToClient :: WAState -> WAClientId -> Error -> IO ()
sendErrorToClient state cid err = case M.lookup cid (clients state) of
                                        Just c -> do
                                                    sendToClient c (WEError err)
                                                    atomically $ webLogErr state err
                                        Nothing -> atomically $ webLogErr state (mkError $ "sending error clientId not found " ++ show cid) -- nothing else to do.

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
