module Load where

import Data.Aeson
import System.IO
import System.IO.Error
import System.FilePath
import Control.Exception
import qualified Data.ByteString.Lazy as B
import Data.Bifunctor (first)

import Paths
import XMapTypes
import Project
import ProjectJson
import View
import Errors

tryReadFile :: FilePath -> IO (Either IOError B.ByteString)
tryReadFile p = try $ B.readFile p

tryReadAndDecode ::  FromJSON a => FilePath -> IO (Either Error a)
tryReadAndDecode p = do
     strOrExc <- tryReadFile p
     return $ case strOrExc of
        Left ex -> Left $ mkError (show ex)
        Right json -> first mkError $ eitherDecode json

loadAvailableProjects :: FilePath -> IO (Either Error AllProjects)
loadAvailableProjects root = tryReadAndDecode (allProjectsPath root)

loadProject :: FilePath -> ProjectName -> IO (Either Error Project)
loadProject root pr = tryReadAndDecode (projectPath root pr)

loadXMap :: FilePath -> ProjectName -> XMapName -> IO (Either Error XNamedMap)
loadXMap root p m = tryReadAndDecode (xMapPath root p m)

loadView :: FilePath -> ProjectName -> ViewName -> IO (Either Error View)
loadView root p n = tryReadAndDecode (viewPath root p n)
