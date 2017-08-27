module Paths where

import System.FilePath

import XMapTypes
import Project
import View
import qualified Data.Text as T

allProjectsPath :: FilePath -> FilePath
allProjectsPath root = root </> "allprojects.ex"

projectDir :: FilePath -> ProjectName -> FilePath
projectDir root (ProjectName projectName) = root </> T.unpack projectName

projectPath :: FilePath -> ProjectName -> FilePath
projectPath root projectName = projectDir root projectName </> "project.ex"

xMapPath :: FilePath -> ProjectName -> XMapName -> FilePath
xMapPath root projectName (XMapName mapName) = projectDir root projectName </> joinPath (map T.unpack mapName) <.> "mp"

viewPath :: FilePath -> ProjectName -> ViewName -> FilePath
viewPath root projectName (ViewName viewName) = projectDir root projectName </> T.unpack viewName <.> "vw"