module Actors where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Map.Strict as M

import XMapTypes
import State

actorCalculation :: TChan CalculationMessage -> RuntimeCalculation -> STM ()
actorCalculation chan calculation = loop
    where loop = do
                    msg <- readTChan chan
                    case msg of
                        (CMMap m) -> do
                            modifyTVar (repository calculation) (updateRepository m)
                            loop
                        CMStop -> return ()
          updateRepository m r = M.insert (xmapName m) (Just (xmap m)) r
