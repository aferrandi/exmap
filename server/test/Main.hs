module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad

import qualified FormulaTest
import qualified FormulaParserTest
import qualified TextEnumsTest
import qualified ProjectJsonTest
import qualified XMapJsonTest
import qualified DependenciesTest
import qualified AssocListTest
import qualified OperationsTest

main :: IO Counts
main = runTestTT tests
       where tests = TestList $
                   AssocListTest.tests
                ++ FormulaTest.tests
                ++ OperationsTest.tests
                ++ FormulaParserTest.tests
                ++ TextEnumsTest.tests
                ++ ProjectJsonTest.tests
                ++ XMapJsonTest.tests
                ++ DependenciesTest.tests