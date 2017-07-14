{-# LANGUAGE ExistentialQuantification #-}
module Formula(XFormula(..)) where

import XMapTypes
import OperationTypes
import ApplicationTypes

data XFormula = XFMap XMapName
            | XFOperation OperationName XFormula XFormula
            | XFApplication ApplicationName XFormula
    deriving (Eq, Show)


