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


startFromRootPath :: String -> String -> IO ()
startFromRootPath index root = do
    print $ "loading index.html from " ++ index ++" and projects from " ++ root
    logChan <- newTChanIO
    _ <- forkIO $ actorLog root logChan
    system <- startSystem root logChan
    ps <- atomically $ readTVar (projectByName system)
    print $ "System loaded with " ++ show (M.size ps) ++ " projects"
    systemChan <- newTChanIO
    _ <- forkIO $ actorSystem systemChan system
    WebApp.runWebApp systemChan logChan index

main :: IO ()
main = do
    args <- getArgs
    case args of
        (index : root : xs) -> startFromRootPath index root
        _ -> do
            print "exmap <indexHtmlPath> <projectsRootPath>"
            return ()


