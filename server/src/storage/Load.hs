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

loadCalculation :: FilePath -> ProjectName -> CalculationName -> IO (Either Error Calculation)
loadCalculation root pn cn = tryReadAndDecode $ calculationPath root pn cn

loadProject :: FilePath -> ProjectName -> IO (Either Error Project)
loadProject root pn = tryReadAndDecode $ projectPath root pn

loadXMap :: FilePath -> ProjectName -> XMapName -> IO (Either Error XNamedMap)
loadXMap root pn mn = tryReadAndDecode $ xMapPath root pn mn

loadView :: FilePath -> ProjectName -> ViewName -> IO (Either Error View)
loadView root pn vn = tryReadAndDecode $ viewPath root pn vn
