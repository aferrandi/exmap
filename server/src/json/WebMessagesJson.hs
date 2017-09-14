{-# LANGUAGE OverloadedStrings   #-}

module WebMessagesJson where

import Data.Aeson
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.Text as T

import WebMessages
import ProjectJson
import qualified Errors as E

instance ToJSON WebEvent where
     toJSON (WEAllProjects ps) = object [ "type" .=  T.pack "allProjects"
                                        , "projectNames" .= ps
                                        ]
     toJSON (WEViewChanged pn vn m) = object [ "type" .=  T.pack "viewChanged"
                                        , "projectName" .= pn
                                        , "viewName" .= vn
                                        , "map" .= m
                                        ]
     toJSON (WEProjectContent p) = object [ "type" .=  T.pack "projectContent"
                                        , "project" .= p
                                        ]
     toJSON (WEProjectStored pn) = object [ "type" .=  T.pack "projectStored"
                                        , "projectName" .= pn
                                        ]
     toJSON (WEMapLoaded pn m) = object [ "type" .=  T.pack "mapLoaded"
                                        , "projectName" .= pn
                                        , "map" .= m
                                        ]
     toJSON (WEMapStored pn mn) = object [ "type" .=  T.pack "mapStored"
                                        , "projectName" .= pn
                                        , "mapName" .= mn
                                        ]
     toJSON (WEUnsubscribedFromView pn vn) = object [ "type" .=  T.pack "unsubscribedFromView"
                                        , "projectName" .= pn
                                        , "viewName" .= vn
                                        ]
     toJSON (WEViewStatus pn v ms) = object [ "type" .=  T.pack "viewStatus"
                                        , "projectName" .= pn
                                        , "view" .= v
                                        , "maps" .= ms
                                        ]
     toJSON (WEError (E.Error err)) = object [ "type" .=  T.pack "error"
                                        , "error" .= err
                                        ]
     toJSON (WEInfo info) = object [ "type" .=  T.pack "info"
                                        , "info" .= info
                                        ]

instance FromJSON WebRequest where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "allProjects") ->  return WRAllProjects
      Just (String "subscribeToProject") ->  WRSubscribeToProject <$> v .: "projectName"
      Just (String "newProject") -> WRNewProject <$> v .: "project"
      Just (String "updateProject") -> WRUpdateProject <$> v .: "project"
      Just (String "loadMap") ->  WRLoadMap <$> v .: "projectName" <*> v .: "mapName"
      Just (String "storeMap") ->  WRStoreMap <$> v .: "projectName" <*> v .: "map"
      Just (String "subscribeToView") -> WRSubscribeToView <$> v .: "projectName" <*> v .: "viewName"
      Just (String "unsubscribeFromView") ->  WRUnsubscribeFromView <$> v .: "projectName" <*> v .: "viewName"
