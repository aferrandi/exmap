module Main where

import System.Environment

import qualified Data.Maybe as B
import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as BL

import SystemState
import SystemBuild
import SystemActor
import LogActor
import qualified WebApp


startFromRootPath :: Int -> String -> String -> IO ()
startFromRootPath port pagesPath root = do
    print $ "loading index.html from " ++ pagesPath ++" and projects from " ++ root ++ ". Server starting on port " ++ (show port)
    logChan <- newTChanIO
    _ <- forkIO $ actorLog root logChan
    system <- startSystem root logChan
    ps <- atomically $ readTVar (projectByName system)
    print $ "System loaded with " ++ show (M.size ps) ++ " projects"
    systemChan <- newTChanIO
    _ <- forkIO $ actorSystem systemChan system
    -- indexContent <- BL.readFile index
    print $ "Web server starting on port " ++ show(port)
    WebApp.runWebApp port systemChan logChan pagesPath

main :: IO ()
main = do
    args <- getArgs
    case args of
        (port : pagesPath : root : xs) ->
            case readMaybe port of
                Just p -> startFromRootPath p pagesPath root
                Nothing -> do
                    print "port must be an integer value"
                    return ()
        _ -> do
            print "exmap <port> <pagesHtmlPath> <projectsRootPath>"
            return ()


