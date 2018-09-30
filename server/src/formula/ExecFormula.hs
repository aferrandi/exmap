module ExecFormula (execFormula) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import XMapTypes
import XFunction
import Operations
import Formula
import FormulaText (mapNameToText)

execFormula :: XFormula ->  XMapByName -> OperationMode -> XMapErr
execFormula xf mbn om = case xf of
    XFMap n -> case M.lookup n mbn of
        Just m -> Right m
        Nothing -> Left (Error $ T.pack ("map " ++ T.unpack (mapNameToText n) ++ " not found"))
    XFOperation fn subfs -> do
        tms <- mapM (\m -> execFormula m mbn om) subfs
        let f = operationRepository fn
        f om tms


