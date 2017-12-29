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
import SystemActor
import LogActor
import qualified WebApp


startFromRootPath :: String -> IO ()
startFromRootPath rt = do
    print $ "loading system from " ++ rt
    logChan <- newTChanIO
    _ <- forkIO $ actorLog logChan
    system <- startSystem rt logChan
    ps <- atomically $ readTVar (projectByName system)
    print $ "System loaded with " ++ show (M.size ps) ++ " projects"
    systemChan <- newTChanIO
    _ <- forkIO $ actorSystem systemChan system
    WebApp.runWebApp systemChan logChan

main :: IO ()
main = do
    args <- getArgs
    case B.listToMaybe args of
        Just rt -> startFromRootPath rt
        Nothing -> do
            print "exmap <rootPath>"
            return ()


