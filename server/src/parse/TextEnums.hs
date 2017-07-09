module TextEnums (enumValues, enumWithTextCI) where

import qualified Data.Char as C
import Data.List (find)

enumValues:: (Enum a, Bounded a) => [a]
enumValues = enumFrom minBound

enumWithTextCI :: (Enum a, Show a) => [a] -> String -> Maybe a
enumWithTextCI es s = find (\e -> tolowerEnum e == toLowerText s) es
    where tolowerEnum e = toLowerText $ show e
          toLowerText = map C.toLower