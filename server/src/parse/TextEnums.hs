module TextEnums (enumValues, enumWithTextCI, showT, readT) where

import qualified Data.Char as C
import qualified Data.Text as T
import Data.List (find)

enumValues:: (Enum a, Bounded a) => [a]
enumValues = enumFrom minBound

enumWithTextCI :: (Enum a, Show a) => [a] -> T.Text -> Maybe a
enumWithTextCI es s = find (\e -> toLowerEnum e == T.toLower s) es
    where toLowerEnum = T.toLower . showT

showT :: Show a => a -> T.Text
showT = T.pack . show

readT :: Read a => T.Text -> a
readT = read . T.unpack