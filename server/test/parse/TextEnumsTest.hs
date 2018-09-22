module TextEnumsTest where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad

import qualified Data.Text as T

import TextEnums
import OperationTypes

enumValues_OperationName_containNegate = TestCase (assertBool "contain negate in operations names" (elem Negate enumValues) )

enumWithTextCI_OperationName_containNegate = TestCase (assertEqual "contains negate in operation names texts" (Just Negate) (enumWithTextCaseInsensitive enumValues (T.pack "negate")))

enumWithTextCI_OperationName_notContainNegate = TestCase (assertEqual "does not contain nogate in operation names texts" Nothing (enumWithTextCaseInsensitive (enumValues :: [OperationName]) (T.pack "nogate") ))
