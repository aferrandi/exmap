module Paths where

import System.FilePath

import XMapTypes
import Project
import View
import qualified Data.Text as T

allProjectsPath :: FilePath -> FilePath
allProjectsPath root = root </> "allProjects.ex"

projectDir :: FilePath -> ProjectName -> FilePath
projectDir root (ProjectName pn) = root </> T.unpack pn

projectPath :: FilePath -> ProjectName -> FilePath
projectPath root pn = projectDir root pn </> "project.ex"

xMapPath :: FilePath -> ProjectName -> XMapName -> FilePath
xMapPath root pn (XMapName mn) = projectDir root pn </> joinPath (map T.unpack mn) <.> "map"

viewPath :: FilePath -> ProjectName -> ViewName -> FilePath
viewPath root pn (ViewName vn) = projectDir root pn </> T.unpack vn <.> "vew"

calculationPath :: FilePath -> ProjectName -> CalculationName -> FilePath
calculationPath root pn (CalculationName cn) = projectDir root pn </> T.unpack cn <.> "clc"
