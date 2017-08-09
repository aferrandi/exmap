module Load where

import Data.Aeson
import System.IO
import System.IO.Error
import System.FilePath
import Control.Exception
import qualified Data.ByteString.Lazy as B

import Paths
import XMapTypes
import Project
import ProjectJson

tryReadFile :: FilePath -> IO (Either IOError B.ByteString)
tryReadFile p = try $ B.readFile p

loadAvailableProjects :: FilePath -> IO (Either String AllProjects)
loadAvailableProjects root = do
     strOrExc <- tryReadFile (allProjectsPath root)
     return $ case strOrExc of
        Left ex -> Left (show ex)
        Right json -> eitherDecode json


loadProject :: FilePath -> ProjectName -> IO (Either String Project)
loadProject root pr = do
     strOrExc <- tryReadFile (projectPath root pr)
     return $ case strOrExc of
        Left ex -> Left (show ex)
        Right json -> eitherDecode json

loadXMap :: FilePath -> XMapName -> IO (Either String XNamedMap)
loadXMap root m = do
     strOrExc <- tryReadFile (xMapPath root m)
     return $ case strOrExc of
        Left ex -> Left (show ex)
        Right json -> eitherDecode json
