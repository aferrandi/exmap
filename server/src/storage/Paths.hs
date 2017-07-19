module Paths where

import System.FilePath

import XMapTypes
import Project
import qualified Data.Text as T

allProjectsPath :: FilePath -> FilePath
allProjectsPath root = root </> "allprojects.txt"

projectPath :: FilePath -> ProjectName -> FilePath
projectPath root (ProjectName projectName) = root </> T.unpack projectName <.> "txt"

xMapPath :: FilePath -> XMapName -> FilePath
xMapPath root (XMapName mapName) = root </> joinPath (map T.unpack mapName) <.> "txt"