module EnumToString exposing (..)

import Dict exposing (Dict, get)
import Json.Decode exposing (..)

stringToEnum : Dict String a -> String -> Decoder a
stringToEnum m s =
    case get s m of
        Just e -> succeed e
        Nothing -> fail <| "Unknown : " ++ s
