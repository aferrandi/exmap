module Main where

import System.Environment

import qualified Data.Maybe as B
import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar

import State
import StateBuild
import qualified WebApp


main :: IO ()
main = do
    args <- getArgs
    case B.listToMaybe args of
        Just root -> do
            print $ "loading system from " ++ root
            system <- startSystem root
            ps <- readTVarIO (projectByName system)
            print $ "System loaded with " ++ show (M.size ps) ++ " projects"
            WebApp.runWebApp
        Nothing -> do
            print "exmap <rootPath>"
            return ()


