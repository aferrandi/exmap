
module Main where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.Maybe
import System.Environment

import qualified WebApp
import XMapTypes
import XFunction
import Formula
import Operations
import FormulaParser
import View
import State
import Project
import ProjectJson
import Load
import CalculationActor
import LogActor

main :: IO ()
main = do
    args <- getArgs
    case listToMaybe args of
        Just root -> do
            let system = loadSystem root
            WebApp.runWebApp
        Nothing -> do
            print "exmap <rootPath>"
            return ()


startSystem :: FilePath -> IO XSystem
startSystem root = do
    ps <- loadSystem root
    let psMap = M.fromList $  map (\p -> (p, Nothing)) ps
    emptyByName <- atomically $ newTVar psMap
    emptyByMapName <- atomically $ newTVar M.empty
    return XSystem {
        projectByName = emptyByName,
        projectByMapName = emptyByMapName
    }

loadSystem :: FilePath -> IO [ProjectName]
loadSystem root = do
    pe <- loadAvailableProjects root
    case pe of
        Right (AllProjects ps) -> return ps
        Left err -> return []
