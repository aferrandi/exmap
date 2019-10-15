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

main :: IO Counts
main = runTestTT tests
       where tests = TestList [
                FormulaTest.execFormula_trivialFormula_originalMap,
                FormulaTest.execFormula_operationFormulaTwoParameters_expectedMap,
                FormulaTest.execFormula_operationFormulaOneParameter_expectedMap,

                FormulaParserTest.parseFormula_formulaWithMap_map,
                FormulaParserTest.parseFormula_formulaOperation_negate,
                FormulaParserTest.parseFormula_formulaWrongOperationName_error,
                FormulaParserTest.parseFormula_formulaOperation_operation,
                FormulaParserTest.parseFormula_complex_operation,
                FormulaParserTest.parseFormula_formulaWithSpaces_map,

                TextEnumsTest.enumValues_OperationName_containNegate,
                TextEnumsTest.enumWithTextCI_OperationName_containNegate,
                TextEnumsTest.enumWithTextCI_OperationName_notContainNegate,

                ProjectJsonTest.toParseJSON_calculation_same,
                ProjectJsonTest.toParseJSON_view_same,
                ProjectJsonTest.toParseJSON_project_same,
                ProjectJsonTest.toParseJSON_allProjects_same,
                ProjectJsonTest.toParseJSON_user_same,
                XMapJsonTest.toParseJSON_doublemap_same,
                XMapJsonTest.toParseJSON_stringmap_same,

                DependenciesTest.formulaDependencies_trivialFormula_originalMap,
                DependenciesTest.formulaDependencies_complexFormula_maps,
                DependenciesTest.formulaDependencies_duplicates_onlyOnce,
                DependenciesTest.viewDependencies_empty_empty,
                DependenciesTest.viewDependencies_complex_maps,
                DependenciesTest.calculationDependencies_simpleCalculation_originalMap,

                AssocListTest.groupAssocListByKey_lookup_gets_list,
                AssocListTest.groupAssocListByKey_final_list_expected
                ]
