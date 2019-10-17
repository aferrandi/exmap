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

formulaDependencies_trivialFormula_originalMap = TestCase (assertEqual "dependencies trivial formula" [ka] (formulaDependencies f))
    where ka = mapName ["a"]
          f = XFMap ka

formulaDependencies_complexFormula_maps = TestCase (assertEqual "dependencies complex formula" [ka, kb] (formulaDependencies f))
    where ka = mapName ["a"]
          kb = mapName ["b"]
          f = XFOperation Ops.Negate [XFOperation Ops.Add [XFMap ka, XFMap kb]]

formulaDependencies_duplicates_onlyOnce = TestCase (assertEqual "dependencies formula with duplicates" [ka] (formulaDependencies f))
    where ka = mapName ["a"]
          f = XFOperation Ops.Add [XFMap ka, XFOperation Ops.Negate [XFMap ka]]

viewDependencies_empty_empty = TestCase (assertEqual "dependencies empty view" [] (viewDependencies v))
    where v = View {
                viewName = ViewName (T.pack "v"),
                rows = []
            }

viewDependencies_complex_maps = TestCase (assertEqual "dependencies complex view" [ka, kb] (viewDependencies v))
    where ka = mapName ["a"]
          kb = mapName ["b"]
          label s = ViewLabel (T.pack s)
          v = View {
                viewName = ViewName (T.pack "v"),
                rows = [
                    ViewRow [ MapItem ka, MapItem kb],
                    ViewRow [ LabelItem (label "l1"), LabelItem (label "l2")]
                ]
            }

calculationDependencies_simpleCalculation_originalMap = TestCase (assertEqual "dependencies simple calculatiom" [ka] (calculationDependencies c))
    where ka = mapName ["a"]
          kr = mapName ["r"]
          c = Calculation {
                calculationName = CalculationName $ T.pack "calc",
                resultName = kr,
                formula = XFMap ka,
                operationMode = Intersection
          }

tests = [
          formulaDependencies_trivialFormula_originalMap,
          formulaDependencies_complexFormula_maps,
          formulaDependencies_duplicates_onlyOnce,
          viewDependencies_complex_maps,
          viewDependencies_empty_empty,
          calculationDependencies_simpleCalculation_originalMap
        ]