module Main where

import System.Environment

import qualified WebApp
import qualified Data.Maybe as M

import StateBuild


main :: IO ()
main = do
    args <- getArgs
    case M.listToMaybe args of
        Just root -> do
            let system = startSystem root
            WebApp.runWebApp
        Nothing -> do
            print "exmap <rootPath>"
            return ()


