module LogTypes where

import qualified Data.Text as T

import Errors

data Log = LogInfo T.Text
           | LogError T.Text

