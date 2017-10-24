{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module WebMessagesJson where

import Data.Aeson
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.Text as T

import WebMessages
import ProjectJson()
import XMapJson()
import qualified Errors as E

instance ToJSON WebEvent where
     toJSON (WEAllProjects ps) = object [ "type" .=  T.pack "allProjects"
                                        , "projectNames" .= ps
                                        ]
     toJSON (WEViewChanged pn vn ms) = object [ "type" .=  T.pack "viewChanged"
                                        , "projectName" .= pn
                                        , "viewName" .= vn
                                        , "maps" .= ms
                                        ]
     toJSON (WEProjectContent p) = object [ "type" .=  T.pack "projectContent"
                                        , "project" .= p
                                        ]
     toJSON (WEProjectStored p) = object [ "type" .=  T.pack "projectStored"
                                        , "project" .= p
                                        ]
     toJSON (WEMapsLoaded pn ms) = object [ "type" .=  T.pack "mapLoaded"
                                        , "projectName" .= pn
                                        , "maps" .= ms
                                        ]
     toJSON (WEMapStored pn mn) = object [ "type" .=  T.pack "mapStored"
                                        , "projectName" .= pn
                                        , "mapName" .= mn
                                        ]
     toJSON (WEUnsubscribedFromView pn vn) = object [ "type" .=  T.pack "unsubscribedFromView"
                                        , "projectName" .= pn
                                        , "viewName" .= vn
                                        ]
     toJSON (WEMapsInProject pn mns) = object [ "type" .=  T.pack "mapsInProject"
                                        , "projectName" .= pn
                                        , "mapNames" .= mns
                                        ]
     toJSON (WEViewStatus pn v ms) = object [ "type" .=  T.pack "viewStatus"
                                        , "projectName" .= pn
                                        , "view" .= v
                                        , "maps" .= ms
                                        ]
     toJSON (WEViewLoaded pn v) = object [ "type" .=  T.pack "viewLoaded"
                                        , "projectName" .= pn
                                        , "view" .= v
                                        ]
     toJSON (WEViewStored pn vn) = object [ "type" .=  T.pack "viewStored"
                                        , "projectName" .= pn
                                        , "viewName" .= vn
                                        ]
     toJSON (WEError (E.Error err)) = object [ "type" .=  T.pack "error"
                                        , "error" .= err
                                        ]
     toJSON (WEInfo info) = object [ "type" .=  T.pack "info"
                                        , "info" .= info
                                        ]
     toJSON (WECalculationLoaded pn cs) = object [ "type" .=  T.pack "calculationLoaded"
                                        , "projectName" .= pn
                                        , "calculationSource" .= cs
                                        ]
     toJSON (WECalculationStored pn cn) = object [ "type" .=  T.pack "calculationStored"
                                        , "projectName" .= pn
                                        , "calculationName" .= cn
                                        ]
     toJSON (WEFunctions fs) = object [ "type" .=  T.pack "functions"
                                        , "functions" .= fs
                                        ]

instance FromJSON WebRequest where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "allProjects") ->  return WRAllProjects
      Just (String "subscribeToProject") ->  WRSubscribeToProject <$> v .: "projectName"
      Just (String "newProject") -> WRNewProject <$> v .: "project"
      Just (String "updateProject") -> WRUpdateProject <$> v .: "project"
      Just (String "loadMaps") ->  WRLoadMaps <$> v .: "projectName" <*> v .: "mapNames"
      Just (String "storeMap") ->  WRStoreMap <$> v .: "projectName" <*> v .: "map"
      Just (String "subscribeToView") -> WRSubscribeToView <$> v .: "projectName" <*> v .: "viewName"
      Just (String "unsubscribeFromView") ->  WRUnsubscribeFromView <$> v .: "projectName" <*> v .: "viewName"
      Just (String "mapsInProject") ->  WRMapsInProject <$> v .: "projectName"
      Just (String "loadView") ->  WRLoadView <$> v .: "projectName" <*> v .: "viewName"
      Just (String "storeView") ->  WRStoreView <$> v .: "projectName" <*> v .: "view"
      Just (String "loadCalculation") ->  WRLoadCalculation <$> v .: "projectName" <*> v .: "calculationName"
      Just (String "storeCalculation") ->  WRStoreCalculation <$> v .: "projectName" <*> v .: "calculationSource"
      Just (String "functions") ->  return WRFunctions
      _ -> mempty
   parseJSON _ = mempty
