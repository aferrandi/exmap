module Server.ServerMessaging exposing (..)

import Json.EncodeWebRequest exposing (..)
import Models.ProjectModel exposing (..)
import Models.WebMessages exposing (..)
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
