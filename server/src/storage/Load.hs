module Load where

import Data.Aeson
import System.IO
import System.IO.Error
import System.FilePath
import qualified Data.ByteString.Lazy as B

import Paths
import XMapTypes
import Project
import ProjectJson

loadAvailableProjects :: FilePath -> IO (Either String AllProjects)
loadAvailableProjects root = do
     json <- B.readFile $ allProjectsPath root
     return $ eitherDecode json

loadProject :: FilePath -> ProjectName -> IO (Either String Project)
loadProject root pr = do
    json <- B.readFile $ projectPath root pr
    return $ eitherDecode json

loadXMap :: FilePath -> XMapName -> IO (Either String XNamedMap)
loadXMap root m = do
    json <- B.readFile $ xMapPath root m
    return $ eitherDecode json
