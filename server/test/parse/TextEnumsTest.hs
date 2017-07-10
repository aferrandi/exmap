module TextEnumsTest where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad

import TextEnums
import Applications

enumValues_ApplicationName_containNegate = TestCase (assertBool "contain negate in application names" (elem Negate enumValues) )

enumWithTextCI_ApplicationName_containNegate = TestCase (assertEqual "contains negate in application names texts" (Just Negate) (enumWithTextCI enumValues "negate"))

enumWithTextCI_ApplicationName_notContainNegate = TestCase (assertEqual "does not contain nogate in application names texts" Nothing (enumWithTextCI (enumValues :: [ApplicationName]) "nogate" ))
