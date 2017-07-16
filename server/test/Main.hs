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

main :: IO Counts
main = runTestTT tests
       where tests = TestList [
                        FormulaTest.apply_trivialFormula_originalMap,
                        FormulaTest.apply_operationFormula_expectedMap,
                        FormulaTest.apply_applicationFormula_expectedMap,

                        FormulaParserTest.parseFormula_formulaWithMap_map,
                        FormulaParserTest.parseFormula_formulaApplication_application,
                        FormulaParserTest.parseFormula_formulaOperation_operation,
                        FormulaParserTest.parseFormula_complex_operation,

                        TextEnumsTest.enumValues_ApplicationName_containNegate,
                        TextEnumsTest.enumWithTextCI_ApplicationName_containNegate,
                        TextEnumsTest.enumWithTextCI_ApplicationName_notContainNegate,

                        FormulaParserTest.parseFormula_formulaWrongApplicationName_error,

                        ProjectJsonTest.toParseJSON_calculation_same,
                        ProjectJsonTest.toParseJSON_project_same,
                        ProjectJsonTest.toParseJSON_user_same
                        ]
