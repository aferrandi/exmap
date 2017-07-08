module ExecFormula (execFormula) where

import XMapTypes
import XFunction
import Operations
import Applications
import XValues
import Formula
import qualified Data.Map.Strict as Map

execFormula :: XFormula ->  XMapByName -> OperationMode -> XMapErr
execFormula xf rm m = case xf of
    (XFMap n) -> case Map.lookup n rm of
        Just om -> Right om
        Nothing -> Left "map not found"
    (XFOperation f a b) -> do
        oa <- execFormula a rm m
        ob <- execFormula b rm m
        let rf = operationRepository f
        rf m oa ob
    (XFApplication f a) -> do
        oa  <- execFormula a rm m
        let rf = applicationRepository f
        rf oa
