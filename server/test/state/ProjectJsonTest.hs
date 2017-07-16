module ProjectJsonTest (toParseJSON_calculation_same, toParseJSON_project_same) where

import Data.Aeson
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import qualified Data.Text as T

import Project
import ProjectJson
import TestTypes
import Formula
import View
import XFunction
import OperationTypes
import Debug.Trace

toParseJSON_calculation_same = TestCase (assertEqual "calculation -> json -> calculation" (Just calculationExample) (decode json) )
        where json = encode calculationExample


calculationExample = Calculation {
        calculationName = CalculationName (T.pack "calc"),
        formula = XFOperation Subtract (XFMap (mapName ["one"])) (XFMap (mapName ["two"])),
        maps = [mapName ["one"], mapName ["two"]],
        operationMode = Intersection
        }

toParseJSON_project_same = TestCase (assertEqual "project -> json -> project" (Just original) (decode json) )
    where original = Project {
            projectName = ProjectName (T.pack "proj"),
            calculations = [calculationExample],
            views = [view],
            sources = [source]
            }
          view = View [
                    ViewRow [
                        MapItem (mapName ["one"]),
                        LabelItem (ViewLabel (T.pack "label"))
                        ]
                    ]
          json = encode original
          source = Source {
              sourceType = InternalSource,
              sourceOfMaps = [mapName ["one"],mapName ["two"]]
          }