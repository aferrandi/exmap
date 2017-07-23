module ProjectJsonTest (toParseJSON_calculation_same, toParseJSON_project_same, toParseJSON_user_same, toParseJSON_map_same) where

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
import XMapTypes
import XFunction
import OperationTypes
import TestTypes
import Debug.Trace

toParseJSON_calculation_same = TestCase (assertEqual "calculation -> json -> calculation" (Just calculationExample) (decode . encodeTrace $ calculationExample) )

toParseJSON_project_same = TestCase (assertEqual "project -> json -> project" (Just original)  (decode . encode $ original) )
    where original = Project {
            projectName = makeProjectName "proj",
            calculations = [calculationExample],
            views = [view],
            sources = [source]
            }
          view = viewExample
          source = sourceExample

toParseJSON_user_same = TestCase (assertEqual "user -> json -> user" (Just original) (decode . encodeTrace $ original) )
    where original =  User {
        userId = T.pack "user",
        accessToProjects = [makeProjectName "proj1", makeProjectName "proj2"]
    }

calculationExample = Calculation {
        resultName = mapName $ ["res"],
        formula = XFOperation Subtract (XFMap . mapName $ ["one"]) (XFMap . mapName $ ["two"]),
        operationMode = Intersection
        }


viewExample = View [
                  ViewRow [
                      MapItem (mapName ["one"]),
                      LabelItem (ViewLabel (T.pack "label"))
                      ]
                  ]

sourceExample = Source {
                  sourceType = InternalSource,
                  sourceOfMaps = [mapName ["one"],mapName ["two"]]
              }


toParseJSON_map_same = TestCase (assertEqual "map -> json -> map" (Just original) (decode . encodeTrace $ original))
    where original =  XNamedMap {
        xmapName = mapName ["map"],
        xmap = singletonXMap "k" 13
    }


makeProjectName :: String -> ProjectName
makeProjectName = ProjectName . T.pack

encodeTrace :: ToJSON a => a -> B.ByteString
encodeTrace = traceJson . encode
    where traceJson json = trace ("json" ++ show json) json