{-# LANGUAGE ExistentialQuantification #-}
module Formula(XFormula(..), applyFormula) where

import XMapTypes
import XFunction
import Operations
import Applications
import XValues
import qualified Data.Map.Strict as Map


data XFormula = XFMap XMapName
            | XFOperation OperationName XFormula XFormula
            | XFApplication ApplicationName XFormula
    deriving (Eq, Show)

applyFormula :: XFormula ->  XMapByName -> OperationMode -> XMapErr
applyFormula xf rm m = case xf of
    (XFMap n) -> case Map.lookup n rm of
        Just om -> Right om
        Nothing -> Left "map not found"
    (XFOperation f a b) -> do
        oa <- applyFormula a rm m
        ob <- applyFormula b rm m
        let rf = operationRepository f
        rf m oa ob
    (XFApplication f a) -> do
        oa  <- applyFormula a rm m
        let rf = applicationRepository f
        rf oa
