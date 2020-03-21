module ProjectJsonTest (tests) where

import Data.Aeson
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

import Project
import ProjectJson
import TestTypes
import Formula
import View
import Calculation
import XMapTypes
import XFunction
import OperationTypes
import Debug.Trace

toParseJSON_calculation_same = TestCase (assertEqual "calculation -> json -> calculation" (Just calculationExample) (decode . encodeTrace $ calculationExample) )

toParseJSON_project_same = TestCase (assertEqual "project -> json -> project" (Just original)  (decode . encodeTrace $ original) )
    where original = Project {
            projectName = makeProjectName "proj",
            calculations = [calculationNameExample],
            views = [viewNameExample],
            sources = [source]
            }
          source = sourceExample


toParseJSON_view_same = TestCase (assertEqual "view -> json -> view" (Just original)  (decode . encodeTrace $ original) )
    where original = View {
                           viewName = viewNameExample,
                           rows = [
                               ViewRow [
                                   MapItem (mapName ["one"]),
                                   LabelItem (ViewLabel (T.pack "label"))
                                   ] RowHasHeader
                           ]
                     }


toParseJSON_allProjects_same = TestCase (assertEqual "allProjects -> json -> allProjects" (Just original) (decode . encodeTrace $ original) )
    where original = AllProjects [makeProjectName "proj1", makeProjectName "proj2"]


toParseJSON_user_same = TestCase (assertEqual "user -> json -> user" (Just original) (decode . encodeTrace $ original) )
    where original =  User {
        userId = T.pack "user",
        accessToProjects = [makeProjectName "proj1", makeProjectName "proj2"]
    }

calculationNameExample = CalculationName (T.pack "calculation")

calculationExample = Calculation {
        calculationName = calculationNameExample,
        resultName = mapName ["res"],
        formula = XFOperation Subtract [XFMap . mapName $ ["one"], XFMap . mapName $ ["two"]],
        operationMode = Intersection
        }


viewNameExample = ViewName (T.pack "view")

sourceExample = Source {
                  sourceType = FileSource,
                  sourceOfMaps = [mapName ["one"],mapName ["two"]]
              }




makeProjectName :: String -> ProjectName
makeProjectName = ProjectName . T.pack

encodeTrace :: ToJSON a => a -> B.ByteString
encodeTrace = traceJson . encode
    where traceJson json = trace ("json" ++ show json) json

tests = [
          toParseJSON_calculation_same,
          toParseJSON_project_same,
          toParseJSON_allProjects_same,
          toParseJSON_user_same,
          toParseJSON_view_same
        ]