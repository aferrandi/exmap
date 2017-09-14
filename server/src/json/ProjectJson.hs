{-# LANGUAGE OverloadedStrings   #-}

module ProjectJson where

import Data.Aeson
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.Text as T
import qualified Data.Map.Strict as M
--import qualified Data.Vector as V

import Project
import XMapTypes
import Formula
import XFunction
import TextEnums
import OperationTypes
import ApplicationTypes
import View

instance FromJSON XMapName where
   parseJSON (String v) = return $ XMapName (T.splitOn "/" v)

instance ToJSON XMapName where
   toJSON (XMapName vs) = String (T.intercalate "/" vs)

instance FromJSON XMapKey where
   parseJSON (String v) = return $ XMapKey v

instance ToJSON XMapKey where
   toJSON (XMapKey v) = String v

instance FromJSON XMap where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "double") -> XMapDouble . M.fromList <$> v .: "values"
      Just (String "int") ->  XMapInt . M.fromList <$> v .: "values"
      Just (String "string") ->  XMapString . M.fromList <$> v .: "values"
      Just (String "bool") ->  XMapBool . M.fromList <$> v .: "values"

instance ToJSON XMap where
     toJSON (XMapDouble values) = object [ "type" .= T.pack "double"
                                          , "values" .= M.toList values
                                          ]
     toJSON (XMapInt values) = object [ "type" .= T.pack "int"
                                       , "values" .= M.toList values
                                       ]
     toJSON (XMapString values) = object [ "type" .=  T.pack "application"
                                          , "values" .= M.toList values
                                          ]
     toJSON (XMapBool values) = object [ "type" .=  T.pack "application"
                                          , "values" .= M.toList values
                                          ]

instance FromJSON XNamedMap where
   parseJSON (Object v) =
      XNamedMap <$> v .: "mapName"
             <*> v .: "xmap"

instance ToJSON XNamedMap where
     toJSON (XNamedMap mapName xmap) =
        object [ "mapName"  .= mapName
               , "xmap"   .= xmap
                 ]

instance FromJSON XFormula where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "map") -> XFMap <$> v .: "name"
      Just (String "operation") ->  XFOperation <$> v .: "name" <*> v .: "formula1" <*> v .: "formula2"
      Just (String "application") ->  XFApplication <$> v .: "name" <*> v .: "formula"

instance ToJSON XFormula where
     toJSON (XFMap n) = object [ "type"  .= T.pack "map"
                              , "name"   .= n
                              ]
     toJSON (XFOperation n f1 f2) = object [ "type" .=  T.pack "operation"
                                            , "name" .= n
                                            , "formula1" .= f1
                                            , "formula2" .= f2
                                            ]
     toJSON (XFApplication n f) = object [ "type"  .=  T.pack "application"
                                            , "name" .= n
                                            , "formula" .= f
                                            ]

instance FromJSON OperationMode where
   parseJSON (String v) = readT <$> pure v

instance ToJSON OperationMode where
   toJSON v = String $ showT v


instance FromJSON OperationName where
   parseJSON (String v) = readT <$> pure v

instance ToJSON OperationName where
   toJSON v = String $ showT v

instance FromJSON ApplicationName where
   parseJSON (String v) = readT <$> pure v

instance ToJSON ApplicationName where
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

instance ToJSON Calculation  where
     toJSON (Calculation calculationName resultName formula operationMode) =
        object [ "calculationName" .= calculationName
               , "resultName" .= resultName
               , "formula" .= formula
               , "operationMode" .= operationMode
                 ]

instance FromJSON SourceType where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "internalSource") -> return InternalSource
      Just (String "odbcSource") ->  OdbcSource <$> v .: "connectionString" <*> v .: "sqlQuery"
      Just (String "httpSource") ->  HttpSource <$> v .: "url"


instance ToJSON SourceType where
     toJSON InternalSource = object [ "type"  .= T.pack "internalSource"                              ]
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

instance ToJSON Source where
     toJSON (Source sourceType sourceOfMaps) =
        object [ "sourceType" .= sourceType
                , "sourceOfMaps" .= sourceOfMaps
                 ]

instance FromJSON ViewLabel where
   parseJSON (String v) = return $ ViewLabel v

instance ToJSON ViewLabel where
   toJSON (ViewLabel v) = String v

instance FromJSON ViewItem where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "map") ->  MapItem <$> v .: "mapName"
      Just (String "label") ->  LabelItem <$> v .: "label"

instance ToJSON ViewItem where
     toJSON (MapItem mapName) = object [ "type" .=  T.pack "map"
                                        , "mapName" .= mapName
                                        ]
     toJSON (LabelItem label) = object [ "type" .=  T.pack "label"
                                        , "label" .= label
                                        ]

instance FromJSON ViewRow where
   parseJSON (Object v) = ViewRow <$> v .: "items"

instance ToJSON ViewRow where
     toJSON (ViewRow is) = object [ "items" .= is]

instance FromJSON ViewName where
   parseJSON (String v) = return $ ViewName v

instance ToJSON ViewName where
   toJSON (ViewName v) = String v

instance FromJSON View where
   parseJSON (Object v) = View <$> v .: "name" <*> v .: "rows"

instance ToJSON View where
     toJSON (View n rs) = object [ "name" .= n
                                 , "rows" .= rs
                                 ]

instance FromJSON ProjectName where
   parseJSON (String v) = return $ ProjectName v

instance ToJSON ProjectName where
   toJSON (ProjectName v) = String v

instance FromJSON Project where
   parseJSON (Object v) =
      Project  <$> v .: "projectName"
             <*> v .: "calculations"
             <*> v .: "viewNames"
             <*> v .: "sources"

instance ToJSON Project where
     toJSON (Project projectName calculations views sources) =
        object [ "projectName"  .= projectName
                , "calculations" .= calculations
                , "viewNames" .= views
                , "sources" .= sources
                 ]

instance FromJSON User where
   parseJSON (Object v) =
      User <$> v .: "userId"
             <*> v .: "accessToProjects"


instance ToJSON User where
     toJSON (User userId accessToProjects) =
        object [ "userId" .= userId
                , "accessToProjects" .= accessToProjects
                 ]

instance FromJSON AllProjects where
   parseJSON  v = AllProjects <$> parseJSONList v


instance ToJSON AllProjects where
     toJSON (AllProjects projectNames) = toJSONList projectNames
