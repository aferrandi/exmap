module FormulaTest(apply_trivialFormula_originalMap, apply_operationFormula_expectedMap, apply_applicationFormula_expectedMap) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Formula
import ExecFormula
import XMapTypes
import qualified XFunction
import qualified Operations as Ops
import qualified Applications as Apps

import TestTypes


apply_trivialFormula_originalMap = TestCase (assertEqual "trivial formula" (Right r) (execFormula f m XFunction.Intersection))
    where r = singletonXMap "k" 13.3
          ka = mapName ["a"]
          m = M.singleton ka r
          f = XFMap ka

apply_operationFormula_expectedMap = TestCase (assertEqual "operation formula" (Right r) (execFormula f m XFunction.Intersection))
    where ka = mapName ["a"]
          kb = mapName ["b"]
          m = M.fromList [(ka ,singletonXMap "k" 13), (kb ,singletonXMap "k" 12)]
          r = singletonXMap "k" 25
          f = XFOperation Ops.Add (XFMap ka) (XFMap kb)

apply_applicationFormula_expectedMap = TestCase (assertEqual "application formula" (Right r) (execFormula f m XFunction.Intersection))
    where ka = mapName ["a"]
          kb = mapName ["b"]
          m = M.fromList [(ka ,singletonXMap "k" 13), (kb ,singletonXMap "k" 12)]
          r = singletonXMap "k" (-13)
          f = XFApplication Apps.Negate (XFMap ka)
