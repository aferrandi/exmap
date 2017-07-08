{-# LANGUAGE ExistentialQuantification #-}
module Formula(XFormula(..)) where

import XMapTypes
import XFunction
import Operations
import Applications



data XFormula = XFMap XMapName
            | XFOperation OperationName XFormula XFormula
            | XFApplication ApplicationName XFormula
    deriving (Eq, Show)


