module CheckProjectContent where

import Control.Concurrent.STM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe as B
import Project
import ProjectState
import Calculation
import View
import XMapTypes

projectContainsResultWithName :: RuntimeProject -> XMapName -> STM (Bool)
projectContainsResultWithName rp mn = do
  cbr <- readTVar $ calculationByResult rp
  return $ M.member mn cbr

projectContainsMapWithName :: RuntimeProject -> XMapName -> STM (Bool)
projectContainsMapWithName rp mn = do
  p <- readTVar $ project rp
  let maps = allMapsInProject p
  return $ B.isJust $ L.find (\md -> (xmapName md) == mn) maps

projectContainsCalculationWithName :: RuntimeProject -> CalculationName -> STM (Bool)
projectContainsCalculationWithName rp cn = do
  p <- readTVar $ project rp
  return $ L.elem cn (calculations p)

projectContainsViewWithName :: RuntimeProject -> ViewName -> STM (Bool)
projectContainsViewWithName rp vn = do
  p <- readTVar $ project rp
  return $ L.elem vn (views p)
