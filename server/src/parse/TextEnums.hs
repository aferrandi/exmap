module TextEnums (enumValues, enumWithTextCaseInsensitive, showT, readT) where

import qualified Data.Text as T
import Data.List (find)
import ShowText

enumValues:: (Enum a, Bounded a) => [a]
enumValues = enumFrom minBound

enumWithTextCaseInsensitive :: (Enum a, Show a) => [a] -> T.Text -> Maybe a
enumWithTextCaseInsensitive es s = find (\e -> toLowerEnum e == T.toLower s) es
    where toLowerEnum = T.toLower . showT
