module AssocList where

import Data.List
import qualified Data.Map.Strict as M

groupAssocListByKey :: Eq k => [(k, a)] -> [(k, [a])]
groupAssocListByKey xs = map oneKeyManyValues $  groupBy equalsKey xs
    where equalsKey (k1, _) (k2, _) = k1 == k2
          oneKeyManyValues :: [(k, a)] -> (k, [a])
          oneKeyManyValues ys = (fst (head ys), map snd ys)

mapWithNothingValues :: Ord k => [k] ->  M.Map k (Maybe a)
mapWithNothingValues = M.fromList . map (\p -> (p, Nothing))
