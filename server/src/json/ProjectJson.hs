{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module ProjectJson where

import Data.Aeson
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.Text as T

import Project
import Calculation
import CalculationJson
import ViewJson
import XMapTypes ()
import Formula
import XFunction
import TextEnums
import OperationTypes
import View
import XMapJson ()


instance FromJSON SourceType where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "fileSource") -> return FileSource
      Just (String "odbcSource") ->  OdbcSource <$> v .: "connectionString" <*> v .: "sqlQuery"
      Just (String "httpSource") ->  HttpSource <$> v .: "url"
      _ -> mempty
   parseJSON _ = mempty


instance ToJSON SourceType where
     toJSON FileSource = object [ "type"  .= T.pack "fileSource"                              ]
     toJSON (OdbcSource connectionString sqlQuery) = object ["type" .= T.pack "odbcSource"
                                            , "connectionString" .= connectionString
                                            , "sqlQuery" .= sqlQuery
                                            ]
     toJSON (HttpSource url) = object [ "type" .= T.pack "httpSource"
                                       , "url" .= url
                                        ]
instance FromJSON Source where
   parseJSON (Object v) =
      Source  <$> v .: "sourceType"
             <*> v .: "sourceOfMaps"
   parseJSON _ = mempty


instance ToJSON Source where
     toJSON (Source sourceType sourceOfMaps) =
        object [ "sourceType" .= sourceType
                , "sourceOfMaps" .= sourceOfMaps
                 ]

instance FromJSON ProjectName where
   parseJSON (String v) = return $ ProjectName v
   parseJSON _ = mempty

instance ToJSON ProjectName where
   toJSON (ProjectName v) = String v

instance FromJSON Project where
   parseJSON (Object v) =
      Project  <$> v .: "projectName"
             <*> v .: "calculations"
             <*> v .: "views"
             <*> v .: "sources"
   parseJSON _ = mempty

instance ToJSON Project where
     toJSON (Project projectName calculations views sources) =
        object [ "projectName"  .= projectName
                , "calculations" .= calculations
                , "views" .= views
                , "sources" .= sources
                 ]

instance FromJSON User where
   parseJSON (Object v) =
      User <$> v .: "userId"
             <*> v .: "accessToProjects"
   parseJSON _ = mempty


instance ToJSON User where
     toJSON (User userId accessToProjects) =
        object [ "userId" .= userId
                , "accessToProjects" .= accessToProjects
                 ]

instance FromJSON AllProjects where
   parseJSON  v = AllProjects <$> parseJSONList v


instance ToJSON AllProjects where
     toJSON (AllProjects projectNames) = toJSONList projectNames
