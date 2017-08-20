module WebClients where

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Aeson

import WebMessages
import WebMessageJson

newtype WAClientId = WAClientId Int
    deriving (Show, Eq, Ord)

data WAClient   = WAClient {
    clientId :: WAClientId,
    connection :: WS.Connection
    }


sendTextToClient :: WAClient -> B.ByteString -> IO ()
sendTextToClient c = WS.sendTextData (connection c)

sendToClient :: WAClient -> WebEvent -> IO ()
sendToClient c e = sendTextToClient c (encode e)

sendToClients :: [WAClient] -> WebEvent -> IO ()
sendToClients cs e = mapM_ (flip sendTextToClient (encode e)) cs