module DependenciesTest (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import qualified Data.Text as T

import Dependencies
import XMapTypes
import TestTypes
import qualified Operations as Ops
import Formula
import View
import XFunction
import Project
import Calculation

formulaDependenciesMaps_trivialFormula_originalMap = TestCase (assertEqual "dependencies trivial formula" [ka] (formulaDependenciesMaps f))
    where ka = makeMapName ["a"]
          f = XFMap ka

formulaDependenciesMaps_complexFormula_maps = TestCase (assertEqual "dependencies complex formula" [ka, kb] (formulaDependenciesMaps f))
    where ka = makeMapName ["a"]
          kb = makeMapName ["b"]
          f = XFOperation Ops.Negate [XFOperation Ops.Add [XFMap ka, XFMap kb]]

formulaDependenciesMaps_duplicates_onlyOnce = TestCase (assertEqual "dependencies formula with duplicates" [ka] (formulaDependenciesMaps f))
    where ka = makeMapName ["a"]
          f = XFOperation Ops.Add [XFMap ka, XFOperation Ops.Negate [XFMap ka]]

viewDependenciesMaps_empty_empty = TestCase (assertEqual "dependencies empty view" [] (viewDependenciesMaps v))
    where v = View {
                viewName = ViewName (T.pack "v"),
                rows = []
            }

viewDependenciesMaps_complex_maps = TestCase (assertEqual "dependencies complex view" [ka, kb] (viewDependenciesMaps v))
    where ka = makeMapName ["a"]
          kb = makeMapName ["b"]
          label s = ViewLabel (T.pack s)
          v = View {
                viewName = ViewName (T.pack "v"),
                rows = [
                    ViewRow [ MapItem ka, MapItem kb] RowHasHeader ,
                    ViewRow [ LabelItem (label "l1"), LabelItem (label "l2")] RowNoHeader
                ]
            }

calculationDependenciesMaps_simpleCalculation_originalMap = TestCase (assertEqual "dependencies simple calculatiom" [ka] (calculationDependenciesMaps c))
    where ka = makeMapName ["a"]
          kr = makeMapName ["r"]
          c = Calculation {
                calculationName = CalculationName $ T.pack "calc",
                resultName = kr,
                resultType = TypeDouble, 
                formula = XFMap ka,
                operationMode = Intersection
          }

tests = [
          formulaDependenciesMaps_trivialFormula_originalMap,
          formulaDependenciesMaps_complexFormula_maps,
          formulaDependenciesMaps_duplicates_onlyOnce,
          viewDependenciesMaps_complex_maps,
          viewDependenciesMaps_empty_empty,
          calculationDependenciesMaps_simpleCalculation_originalMap
        ]