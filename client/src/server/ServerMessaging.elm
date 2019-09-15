module ServerMessaging exposing (..)

import EncodeWebRequest exposing (..)
import ProjectModel exposing (..)
import WebMessages exposing (..)
import Ports exposing(..)

sendToServer : WebRequest -> Cmd Msg
sendToServer req =
    Ports.sendMessage (encodeWebRequest req)


subscriptionsToServer : Model -> Sub Msg
subscriptionsToServer model =
    Ports.onServerMessage Receive

onConnected : Model -> Sub Msg
onConnected model =
   Ports.onWebSocketChange WebSocketConnected
