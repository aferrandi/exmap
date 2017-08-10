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

tryReadAndDecode ::  FromJSON a => FilePath -> IO (Either String a)
tryReadAndDecode p = do
     strOrExc <- tryReadFile p
     return $ case strOrExc of
        Left ex -> Left (show ex)
        Right json -> eitherDecode json

loadAvailableProjects :: FilePath -> IO (Either String AllProjects)
loadAvailableProjects root = tryReadAndDecode (allProjectsPath root)

loadProject :: FilePath -> ProjectName -> IO (Either String Project)
loadProject root pr = tryReadAndDecode (projectPath root pr)

loadXMap :: FilePath -> ProjectName -> XMapName -> IO (Either String XNamedMap)
loadXMap root p m = tryReadAndDecode (xMapPath root p m)
