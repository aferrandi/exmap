module Transform.TypeConversion exposing (..)

import List.Extra as ListX
import Types.XMapTypes exposing (XMapType(..))

textToEnum : List a -> List String -> String -> Maybe a
textToEnum enums texts text =
    ListX.zip enums texts |> ListX.find (\(_, t) -> t == text) |> Maybe.map Tuple.first

enumToText : List a -> List String -> a -> Maybe String
enumToText enums texts enum =
    ListX.zip enums texts |> ListX.find (\(e, _) -> e == enum) |> Maybe.map Tuple.second

xmapTypeToText : XMapType -> Maybe String
xmapTypeToText t = enumToText [TypeDouble, TypeInt, TypeString, TypeBool, TypeDate] ["Double", "Int", "String", "Bool", "Date"] t
