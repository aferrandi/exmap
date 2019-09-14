port module Ports exposing (onServerMessage, onWebSocketChange, sendMessage)

import Json.Encode as JE

-- JAVASCRIPT --> ELM


port onWebSocketChange : (Bool -> msg) -> Sub msg


port onServerMessage : (String -> msg) -> Sub msg



-- ELM --> JAVASCRIPT


port messages : JE.Value -> Cmd msg


sendMessage : JE.Value -> Cmd msg
sendMessage message =
    messages message