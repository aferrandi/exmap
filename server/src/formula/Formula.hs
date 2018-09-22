module Formula(XFormula(..)) where

import XMapTypes
import OperationTypes

data XFormula = XFMap XMapName
            | XFOperation OperationName [XFormula]
    deriving (Eq, Show)


