module FormulaParserTest where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad

import Formula
import FormulaParser
import XMapTypes
import Operations as Ops
import Applications as Apps
import qualified Data.Map.Strict as Map


parseFormula_formulaWithMap_map = TestCase (assertEqual "parse formula with map" (Right res) parsed)
    where parsed = parseFormula "one/map"
          res = XFMap (XMapName ["one", "map"])

parseFormula_formulaApplication_application = TestCase (assertEqual "parse application" (Right res) parsed)
    where parsed = parseFormula "negate one/map"
          res = XFApplication Negate $ XFMap (XMapName ["one", "map"])

parseFormula_formulaWrongApplicationName_error = TestCase (assertEqual "parse application with wrong name" (Left "endOfInput") parsed)
    where parsed = parseFormula "nogate one/map"

parseFormula_formulaOperation_operation = TestCase (assertEqual "parse operation" (Right res) parsed)
    where parsed = parseFormula "subtract one/map two"
          res = XFOperation Subtract (XFMap (XMapName ["one", "map"])) (XFMap (XMapName ["two"]))

parseFormula_complex_operation = TestCase (assertEqual "parse operation" (Right res) parsed)
    where parsed = parseFormula "subtract (negate one/map) two"
          res = XFOperation Subtract (XFApplication Negate $ XFMap (XMapName ["one", "map"])) (XFMap (XMapName ["two"]))



