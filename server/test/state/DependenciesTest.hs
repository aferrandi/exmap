module DependenciesTest (formulaDependencies_trivialFormula_originalMap, formulaDependencies_complexFormula_maps, formulaDependencies_duplicates_onlyOnce) where

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
import qualified Applications as Apps
import Formula


formulaDependencies_trivialFormula_originalMap = TestCase (assertEqual "dependencies trivial formula" [ka] (formulaDependencies f))
    where ka = mapName ["a"]
          f = XFMap ka

formulaDependencies_complexFormula_maps = TestCase (assertEqual "dependencies complex formula" [ka, kb] (formulaDependencies f))
    where ka = mapName ["a"]
          kb = mapName ["b"]
          f = XFApplication Apps.Negate (XFOperation Ops.Add (XFMap ka) (XFMap kb))

formulaDependencies_duplicates_onlyOnce = TestCase (assertEqual "dependencies formula with duplicates" [ka] (formulaDependencies f))
    where ka = mapName ["a"]
          f = XFOperation Ops.Add (XFMap ka) (XFApplication Apps.Negate(XFMap ka))
