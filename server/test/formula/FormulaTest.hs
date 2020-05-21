module FormulaTest(tests) where

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
          ka = makeMapName ["a"]
          m = M.singleton ka r
          f = XFMap ka

execFormula_operationFormulaTwoParameters_expectedMap = TestCase (assertEqual "exec operation two parameters formula" (Right r) (execFormula f m XFunction.Intersection))
    where ka = makeMapName ["a"]
          kb = makeMapName ["b"]
          m = M.fromList [(ka ,makeDoubleXMap [("k",13)]), (kb ,makeDoubleXMap [("k",12)])]
          r = makeDoubleXMap [("k",25)]
          f = XFOperation Ops.Add [XFMap ka, XFMap kb]

execFormula_operationFormulaOneParameter_expectedMap = TestCase (assertEqual "exec operation one parameter formula" (Right r) (execFormula f m XFunction.Intersection))
    where ka = makeMapName ["a"]
          kb = makeMapName ["b"]
          m = M.fromList [(ka ,makeDoubleXMap [("k",13)]), (kb ,makeDoubleXMap [("k",12)])]
          r = makeDoubleXMap [("k",-13)]
          f = XFOperation Ops.Negate [XFMap ka]

formulaResultType_double_double = TestCase (assertEqual "resul type double to touble" (Right TypeDouble) (formulaResultType f m))
    where ka = makeMapName ["a"]
          m = M.fromList [(ka ,TypeDouble)]
          f = XFOperation Ops.Negate [XFMap ka]

formulaResultType_any_string = TestCase (assertEqual "resul type any to string" (Right TypeText) (formulaResultType f m))
    where ka = makeMapName ["a"]
          m = M.fromList [(ka ,TypeDouble)]
          f = XFOperation Ops.ToString [XFMap ka]

formulaResultType_bool_any_any = TestCase (assertEqual "resul type bool + any to any" (Right TypeDouble) (formulaResultType f m))
    where ka = makeMapName ["a"]
          kb = makeMapName ["b"]
          m = M.fromList [(ka ,TypeBool),(kb ,TypeDouble)]
          f = XFOperation Ops.IfThen [XFMap ka, XFMap kb]

tests = [execFormula_trivialFormula_originalMap,
  execFormula_operationFormulaTwoParameters_expectedMap,
  execFormula_operationFormulaOneParameter_expectedMap,
  formulaResultType_double_double,
  formulaResultType_any_string,
  formulaResultType_bool_any_any]