module Load where

import Data.Aeson
import Control.Exception
import qualified Data.ByteString.Lazy as B
import Data.Bifunctor (first)

import Paths
import XMapTypes
import Project
import View
import Calculation
import Errors
import ProjectJson()

tryReadFile :: FilePath -> IO (Either IOError B.ByteString)
tryReadFile p = try $ B.readFile p

tryReadAndDecode ::  FromJSON a => FilePath -> IO (Either Error a)
tryReadAndDecode p = do
     strOrExc <- tryReadFile p
     return $ case strOrExc of
        Left ex -> Left $ readingError ex
        Right js -> first decodingError $ eitherDecode js
     where decodingError ex = mkError $ "Decoding the file " ++ show p ++ "got " ++show ex
           readingError ex = mkError $ "Reading the file " ++ show p ++ "got " ++show ex

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
