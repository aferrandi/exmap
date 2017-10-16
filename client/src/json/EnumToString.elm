module EnumToString exposing (..)

import Json.Decode exposing (..)
import Dict exposing (Dict, get)

stringToEnum : Dict String a -> String -> Decoder a
stringToEnum m s = case get s m of
                        Just e -> succeed e
                        Nothing -> fail <| "Unknown : " ++ s