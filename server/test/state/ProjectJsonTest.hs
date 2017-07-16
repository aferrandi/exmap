module ProjectJsonTest (toParseJSON_calculation_same, toParseJSON_project_same, toParseJSON_user_same) where

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

toParseJSON_calculation_same = TestCase (assertEqual "calculation -> json -> calculation" (Just calculationExample) (decode . encode $ calculationExample) )

toParseJSON_project_same = TestCase (assertEqual "project -> json -> project" (Just original)  (decode . encode $ original) )
    where original = Project {
            projectName = makeProjectName "proj",
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
          source = Source {
              sourceType = InternalSource,
              sourceOfMaps = [mapName ["one"],mapName ["two"]]
          }

toParseJSON_user_same = TestCase (assertEqual "user -> json -> user" (Just original) (decode . encode $ original) )
    where original =  User {
        userId = T.pack "user",
        accessToProjects = [makeProjectName "proj1", makeProjectName "proj2"]
    }

calculationExample = Calculation {
        calculationName = CalculationName (T.pack "calc"),
        formula = XFOperation Subtract (XFMap . mapName $ ["one"]) (XFMap . mapName $ ["two"]),
        maps = [mapName ["one"], mapName ["two"]],
        operationMode = Intersection
        }

makeProjectName :: String -> ProjectName
makeProjectName = ProjectName . T.pack

