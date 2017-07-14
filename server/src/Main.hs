
module Main where

import qualified WebApp
import XMapTypes
import XFunction
import Formula
import Operations
import FormulaParser
import View
import State
import ProjectJson

main :: IO ()
main = WebApp.runWebApp

