module Applications(ApplicationName(..), ApplicationFun, applicationRepository) where

import XFunction
import XMapTypes
import XValues

data ApplicationName = Negate
    deriving (Bounded, Enum, Show, Eq, Read)

type ApplicationFun = XMap -> XMapErr

negatea :: XMap -> XMapErr
negatea = XFunction.apply negatev
    where negatev :: Double -> Double
          negatev = Prelude.negate


applicationRepository :: ApplicationName -> ApplicationFun
applicationRepository Negate = negatea
