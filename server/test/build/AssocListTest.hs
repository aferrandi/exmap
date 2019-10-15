module AssocListTest (
    groupAssocListByKey_lookup_gets_list,
    groupAssocListByKey_final_list_expected
    ) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.Map.Strict as M

import AssocList

groupAssocListByKey_lookup_gets_list = TestCase (assertEqual "can find a letter with 2 numbers" (Just [1, 3]) (M.lookup "a" numbersByLetter))
    where numbersByLetter = M.fromList $ groupAssocListByKey [("a", 1), ("b", 2), ("a", 3)]

groupAssocListByKey_final_list_expected = TestCase (assertEqual "can find a letter with 2 numbers" [("a", [1, 3]), ("b", [2])]  numbersByLetter)
    where numbersByLetter = groupAssocListByKey [("a", 1), ("b", 2), ("a", 3)]
