module Load where

import Data.Aeson
import System.IO
import System.IO.Error
import System.FilePath
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

import XMapTypes
import Project
import ProjectJson


loadAvailableProjects :: FilePath -> IO (Either String AllProjects)
loadAvailableProjects root = do
     let completePath = root </> "allprojects.txt"
     json <- B.readFile completePath
     return $ eitherDecode json

loadProject :: FilePath -> ProjectName -> IO (Either String Project)
loadProject root (ProjectName projectName) = do
    let completePath = root </> (T.unpack projectName) <.> "txt"
    json <- B.readFile completePath
    return $ eitherDecode json

loadXMap :: FilePath -> XMapName -> IO (Either String XNamedMap)
loadXMap root (XMapName mapName) = do
    let completePath = root </> (joinPath (map T.unpack mapName)) <.> "txt"
    json <- B.readFile completePath
    return $ eitherDecode json