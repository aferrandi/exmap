module Store where

import Data.Aeson
import System.IO
import System.IO.Error
import System.FilePath
import qualified Data.ByteString.Lazy as B

import Paths
import XMapTypes
import Project
import ProjectJson


saveAvailableProjects :: FilePath -> AllProjects -> IO ()
saveAvailableProjects root ap = do
    let path = allProjectsPath root
    B.writeFile path (encode ap)

saveProject :: FilePath -> Project -> IO ()
saveProject root pr = do
    let path = projectPath root (projectName pr)
    B.writeFile path (encode pr)

saveXMap :: FilePath -> XNamedMap -> IO ()
saveXMap root m = do
    let path = xMapPath root (xmapName m)
    B.writeFile path (encode m)
