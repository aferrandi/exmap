module ServerMessaging exposing (..)

import WebSocket exposing (..)
import ProjectModel exposing (..)
import WebMessages exposing (..)
import Json.Encode exposing (encode)
import EncodeWebRequest exposing (..)

wsUrl : String
wsUrl = "ws://localhost:3000"

sendToServer : WebRequest -> Cmd Msg
sendToServer req = WebSocket.send wsUrl (encode 0 (encodeWebRequest req))

subscriptionsToServer : Model -> Sub Msg
subscriptionsToServer model =
  WebSocket.listen wsUrl Receive

