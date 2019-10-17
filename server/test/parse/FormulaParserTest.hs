module FormulaParserTest(tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Formula
import FormulaParser
import XMapTypes
import Operations as Ops
import Calculation as Calc

import TestTypes

parseFormula_formulaWithMap_map = TestCase (assertEqual "parse formula with map" (Right res) parsed)
    where parsed = parseFormula (Calc.CalculationFormulaText $ T.pack "one/map")
          res = XFMap (mapName ["one", "map"])

parseFormula_formulaOperation_negate = TestCase (assertEqual "parse operation" (Right res) parsed)
    where parsed = parseFormula (Calc.CalculationFormulaText $ T.pack "negate one/map")
          res = XFOperation Negate [XFMap (mapName ["one", "map"])]

parseFormula_formulaWrongOperationName_error = TestCase (assertEqual "parse operation with wrong name" (Left "endOfInput") parsed)
    where parsed = parseFormula (Calc.CalculationFormulaText $ T.pack "nogate one/map")

parseFormula_formulaOperation_operation = TestCase (assertEqual "parse operation" (Right res) parsed)
    where parsed = parseFormula (Calc.CalculationFormulaText $ T.pack "subtract one/map two")
          res = XFOperation Subtract [XFMap (mapName ["one", "map"]), XFMap (mapName["two"])]

parseFormula_complex_operation = TestCase (assertEqual "parse operation" (Right res) parsed)
    where parsed = parseFormula (Calc.CalculationFormulaText $ T.pack "subtract (negate one/map) two")
          res = XFOperation Subtract [XFOperation Negate [XFMap (mapName ["one", "map"])],  XFMap (mapName ["two"]) ]

parseFormula_formulaWithSpaces_map = TestCase (assertEqual "parse formula with spaces" (Right res) parsed)
    where parsed = parseFormula (Calc.CalculationFormulaText $ T.pack "  negate  one/map  ")
          res = XFOperation Negate [XFMap (mapName ["one", "map"])]

tests = [
          parseFormula_formulaWithMap_map,
          parseFormula_formulaOperation_negate,
          parseFormula_formulaWrongOperationName_error,
          parseFormula_formulaOperation_operation,
          parseFormula_complex_operation,
          parseFormula_formulaWithSpaces_map
        ]