module FormulaTest(execFormula_trivialFormula_originalMap, execFormula_operationFormulaTwoParameters_expectedMap, execFormula_operationFormulaOneParameter_expectedMap) where

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

import TestTypes


execFormula_trivialFormula_originalMap = TestCase (assertEqual "exec trivial formula" (Right r) (execFormula f m XFunction.Intersection))
    where r = makeDoubleXMap [("k",13.3)]
          ka = mapName ["a"]
          m = M.singleton ka r
          f = XFMap ka

execFormula_operationFormulaTwoParameters_expectedMap = TestCase (assertEqual "exec operation two parameters formula" (Right r) (execFormula f m XFunction.Intersection))
    where ka = mapName ["a"]
          kb = mapName ["b"]
          m = M.fromList [(ka ,makeDoubleXMap [("k",13)]), (kb ,makeDoubleXMap [("k",12)])]
          r = makeDoubleXMap [("k",25)]
          f = XFOperation Ops.Add [XFMap ka, XFMap kb]

execFormula_operationFormulaOneParameter_expectedMap = TestCase (assertEqual "exec operation one parameter formula" (Right r) (execFormula f m XFunction.Intersection))
    where ka = mapName ["a"]
          kb = mapName ["b"]
          m = M.fromList [(ka ,makeDoubleXMap [("k",13)]), (kb ,makeDoubleXMap [("k",12)])]
          r = makeDoubleXMap [("k",-13)]
          f = XFOperation Ops.Negate [XFMap ka]

