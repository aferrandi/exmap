module Store where

import Data.Aeson
import System.IO
import System.IO.Error
import System.FilePath
import Control.Exception (try)
import qualified Data.ByteString.Lazy as B

import Paths
import XMapTypes
import Project
import ProjectJson

tryWriteFile :: FilePath -> B.ByteString -> IO (Maybe String)
tryWriteFile p c = do
                    eitherErr <- tryWriteFileAsEither
                    return $ case eitherErr of
                        Left e -> Just (show e)
                        Right _ -> Nothing
    where tryWriteFileAsEither :: IO (Either IOError ())
          tryWriteFileAsEither = try $ B.writeFile p c


saveAvailableProjects :: FilePath -> AllProjects -> IO (Maybe String)
saveAvailableProjects root ap = do
    let path = allProjectsPath root
    tryWriteFile path (encode ap)

saveProject :: FilePath -> Project -> IO (Maybe String)
saveProject root pr = do
    let path = projectPath root (projectName pr)
    tryWriteFile path (encode pr)

saveXMap :: FilePath -> ProjectName -> XNamedMap -> IO (Maybe String)
saveXMap root pn m = do
    let path = xMapPath root pn (xmapName m)
    tryWriteFile path (encode m)
