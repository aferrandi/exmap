module FormulaTest(apply_trivialFormula_originalMap, apply_operationFormula_expectedMap, apply_applicationFormula_expectedMap) where

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
import qualified Data.Map.Strict as Map

singletonMap k n = XMapDouble $ Map.singleton (XMapKey k) n

apply_trivialFormula_originalMap = TestCase (assertEqual "trivial formula" (Right r) (execFormula f m XFunction.Intersection))
    where r = singletonMap "k" 13.3
          ka = XMapName ["a"]
          m = Map.singleton ka r
          f = XFMap ka

apply_operationFormula_expectedMap = TestCase (assertEqual "operation formula" (Right r) (execFormula f m XFunction.Intersection))
    where ka = XMapName ["a"]
          kb = XMapName ["b"]
          m = Map.fromList [(ka ,singletonMap "k" 13), (kb ,singletonMap "k" 12)]
          r = singletonMap "k" 25
          f = XFOperation Ops.Add (XFMap ka) (XFMap kb)

apply_applicationFormula_expectedMap = TestCase (assertEqual "application formula" (Right r) (execFormula f m XFunction.Intersection))
    where ka = XMapName ["a"]
          kb = XMapName ["b"]
          m = Map.fromList [(ka ,singletonMap "k" 13), (kb ,singletonMap "k" 12)]
          r = singletonMap "k" (-13)
          f = XFApplication Apps.Negate (XFMap ka)
