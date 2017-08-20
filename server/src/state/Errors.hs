module Errors where

import qualified Data.Text as T

newtype Error =  Error T.Text
    deriving Show

mkError :: String -> Error
mkError s = Error (T.pack s)