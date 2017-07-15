module ProjectJsonTest (toParseJSON_calculation_same) where

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
import XFunction
import OperationTypes
import Debug.Trace

toParseJSON_calculation_same = TestCase (assertEqual "parse formula with map" (Just original) (decode json) )
    where original = Calculation {
            calculationName = CalculationName (T.pack "calc"),
            formula = XFOperation Subtract (XFMap (mapName ["one"])) (XFMap (mapName ["two"])),
            maps = [mapName ["one"], mapName ["two"]],
            operationMode = Intersection
            }
          json = encode original