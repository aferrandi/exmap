module Main where

import System.Environment

import qualified Data.Maybe as B
import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent

import SystemState
import SystemBuild
import Store
import SystemActor
import qualified WebApp


main :: IO ()
main = do
    args <- getArgs
    case B.listToMaybe args of
        Just root -> do
            print $ "loading system from " ++ root
            system <- startSystem root
            ps <- atomically $ readTVar (projectByName system)
            print $ "System loaded with " ++ show (M.size ps) ++ " projects"
            chanSystem <- newTChanIO
            forkIO $ actorSystem chanSystem system
            WebApp.runWebApp chanSystem
        Nothing -> do
            print "exmap <rootPath>"
            return ()


