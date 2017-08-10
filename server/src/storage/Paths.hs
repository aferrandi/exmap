module Paths where

import System.FilePath

import XMapTypes
import Project
import qualified Data.Text as T

allProjectsPath :: FilePath -> FilePath
allProjectsPath root = root </> "allprojects.txt"

projectDir :: FilePath -> ProjectName -> FilePath
projectDir root (ProjectName projectName) = root </> T.unpack projectName

projectPath :: FilePath -> ProjectName -> FilePath
projectPath root projectName = (projectPath root projectName) </> "project.txt"

xMapPath :: FilePath -> ProjectName -> XMapName -> FilePath
xMapPath root projectName (XMapName mapName) = (projectPath root projectName) </> joinPath (map T.unpack mapName) <.> "txt"