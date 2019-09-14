module ServerMessaging exposing (..)

import EncodeWebRequest exposing (..)
import Json.Encode exposing (encode)
import ProjectModel exposing (..)
import WebMessages exposing (..)
import Ports exposing(..)



wsUrl : String
wsUrl = "ws://localhost:3000"


sendToServer : WebRequest -> Cmd Msg
sendToServer req =
    Ports.sendMessage (encodeWebRequest req)


subscriptionsToServer : Model -> Sub Msg
subscriptionsToServer model =
    Ports.onServerMessage Receive

onConnected : Model -> Sub Msg
onConnected model =
   Ports.onWebSocketChange WebSocketConnected
