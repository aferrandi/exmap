module FormulaParserTest where

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
import Applications as Apps

import TestTypes

parseFormula_formulaWithMap_map = TestCase (assertEqual "parse formula with map" (Right res) parsed)
    where parsed = parseFormula (T.pack "one/map")
          res = XFMap (mapName ["one", "map"])

parseFormula_formulaApplication_application = TestCase (assertEqual "parse application" (Right res) parsed)
    where parsed = parseFormula (T.pack "negate one/map")
          res = XFApplication Negate $ XFMap (mapName ["one", "map"])

parseFormula_formulaWrongApplicationName_error = TestCase (assertEqual "parse application with wrong name" (Left "endOfInput") parsed)
    where parsed = parseFormula (T.pack "nogate one/map")

parseFormula_formulaOperation_operation = TestCase (assertEqual "parse operation" (Right res) parsed)
    where parsed = parseFormula (T.pack "subtract one/map two")
          res = XFOperation Subtract (XFMap (mapName ["one", "map"])) (XFMap (mapName["two"]))

parseFormula_complex_operation = TestCase (assertEqual "parse operation" (Right res) parsed)
    where parsed = parseFormula (T.pack "subtract (negate one/map) two")
          res = XFOperation Subtract (XFApplication Negate $ XFMap (mapName ["one", "map"])) (XFMap (mapName ["two"]))



