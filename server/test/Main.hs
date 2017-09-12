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
import qualified DependenciesTest

main :: IO Counts
main = runTestTT tests
       where tests = TestList [
                FormulaTest.execFormula_trivialFormula_originalMap,
                FormulaTest.execFormula_operationFormula_expectedMap,
                FormulaTest.execFormula_applicationFormula_expectedMap,

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
                ProjectJsonTest.toParseJSON_allProjects_same,
                ProjectJsonTest.toParseJSON_user_same,
                ProjectJsonTest.toParseJSON_map_same,

                DependenciesTest.formulaDependencies_trivialFormula_originalMap,
                DependenciesTest.formulaDependencies_complexFormula_maps,
                DependenciesTest.formulaDependencies_duplicates_onlyOnce,
                DependenciesTest.viewDependencies_empty_empty,
                DependenciesTest.viewDependencies_complex_maps,
                DependenciesTest.calculationDependencies_simpleCalculation_originalMap
                ]
