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
import XMapTypes ()
import Formula
import XFunction
import TextEnums
import OperationTypes
import View
import XMapJson ()

instance FromJSON XFormula where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "map") -> XFMap <$> v .: "name"
      Just (String "operation") ->  XFOperation <$> v .: "name" <*> v .: "formulas"
      _ -> mempty
   parseJSON _ = mempty

instance ToJSON XFormula where
     toJSON (XFMap n) = object [ "type"  .= T.pack "map"
                              , "name"   .= n
                              ]
     toJSON (XFOperation n fs) = object [ "type" .=  T.pack "operation"
                                            , "name" .= n
                                            , "formulas" .= fs
                                            ]
instance FromJSON OperationMode where
   parseJSON (String v) = readT <$> pure v
   parseJSON _ = mempty

instance ToJSON OperationMode where
   toJSON v = String $ showT v


instance FromJSON OperationName where
   parseJSON (String v) = readT <$> pure v
   parseJSON _ = mempty

instance ToJSON OperationName where
   toJSON v = String $ showT v

instance FromJSON CalculationName where
   parseJSON (String v) = return $ CalculationName v

instance ToJSON CalculationName where
   toJSON (CalculationName v) = String v

instance FromJSON Calculation  where
   parseJSON (Object v) =
      Calculation  <$> v .: "calculationName"
             <*> v .: "resultName"
             <*> v .: "formula"
             <*> v .: "operationMode"
   parseJSON _ = mempty

instance ToJSON Calculation  where
     toJSON (Calculation calculationName resultName formula operationMode) =
        object [ "calculationName" .= calculationName
               , "resultName" .= resultName
               , "formula" .= formula
               , "operationMode" .= operationMode
                 ]

instance FromJSON CalculationFormulaText where
   parseJSON (String v) = return $ CalculationFormulaText v

instance ToJSON CalculationFormulaText where
   toJSON (CalculationFormulaText v) = String v

instance FromJSON CalculationSource  where
   parseJSON (Object v) =
      CalculationSource <$> v .: "calculationName"
             <*> v .: "resultName"
             <*> v .: "formulaText"
             <*> v .: "operationMode"
   parseJSON _ = mempty

instance ToJSON CalculationSource where
     toJSON (CalculationSource calculationName resultName formulaText operationMode) =
        object [ "calculationName" .= calculationName
               , "resultName" .= resultName
               , "formulaText" .= formulaText
               , "operationMode" .= operationMode
                 ]

instance ToJSON Functions where
     toJSON (Functions operations) =
        object [ "operations" .= operations ]

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

instance FromJSON ViewLabel where
   parseJSON (String v) = return $ ViewLabel v
   parseJSON _ = mempty

instance ToJSON ViewLabel where
   toJSON (ViewLabel v) = String v

instance FromJSON ViewItem where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "map") ->  MapItem <$> v .: "mapName"
      Just (String "label") ->  LabelItem <$> v .: "label"
      _ -> mempty
   parseJSON _ = mempty

instance ToJSON ViewItem where
     toJSON (MapItem mapName) = object [ "type" .=  T.pack "map"
                                        , "mapName" .= mapName
                                        ]
     toJSON (LabelItem label) = object [ "type" .=  T.pack "label"
                                        , "label" .= label
                                        ]

instance FromJSON ViewRow where
   parseJSON (Object v) = ViewRow <$> v .: "items"
   parseJSON _ = mempty

instance ToJSON ViewRow where
    toJSON (ViewRow is) = object [ "items" .= is]

instance FromJSON ViewName where
   parseJSON (String v) = return $ ViewName v
   parseJSON _ = mempty

instance ToJSON ViewName where
   toJSON (ViewName v) = String v

instance FromJSON View where
   parseJSON (Object v) = View <$> v .: "viewName" <*> v .: "rows"
   parseJSON _ = mempty

instance ToJSON View where
     toJSON (View n rs) = object [ "viewName" .= n
                                 , "rows" .= rs
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
