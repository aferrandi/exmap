module ExecFormula (execFormula) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import XMapTypes
import XFunction
import Operations
import Applications
import XValues
import Formula

execFormula :: XFormula ->  XMapByName -> OperationMode -> XMapErr
execFormula xf rm m = case xf of
    (XFMap n) -> case Map.lookup n rm of
        Just om -> Right om
        Nothing -> Left (T.pack "map not found")
    (XFOperation f a b) -> do
        oa <- execFormula a rm m
        ob <- execFormula b rm m
        let rf = operationRepository f
        rf m oa ob
    (XFApplication f a) -> do
        oa  <- execFormula a rm m
        let rf = applicationRepository f
        rf oa
