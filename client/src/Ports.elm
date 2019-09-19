port module Ports exposing (onServerMessage, onWebSocketChange, sendMessage, connect)

import Json.Encode as JE

-- JAVASCRIPT --> ELM
port onWebSocketChange : (Bool -> msg) -> Sub msg

port onServerMessage : (String -> msg) -> Sub msg

-- ELM --> JAVASCRIPT
port messages : JE.Value -> Cmd msg

port connection : JE.Value -> Cmd msg

connect : JE.Value -> Cmd msg
connect port_ = connection port_

sendMessage : JE.Value -> Cmd msg
sendMessage message = messages message