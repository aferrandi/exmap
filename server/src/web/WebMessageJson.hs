{-# LANGUAGE OverloadedStrings   #-}
module WebMessageJson where

import Data.Aeson
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import ProjectJson
import WebMessages

instance FromJSON WebRequest where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "loadProject") ->  WRLoadProject <$> v .: "projectName"
      Just (String "storeProject") -> WRStoreProject <$> v .: "project"
      Just (String "loadMap") ->  WRLoadMap <$> v .: "projectName" <*> v .: "mapName"
      Just (String "storeMap") ->  WRStoreMap <$> v .: "projectName" <*> v .: "map"
      Just (String "subscribeToView") -> WRSubscribeToView <$> v .: "projectName" <*> v .: "viewName"
      Just (String "unSubscribeFromView") ->  WRUnSubscribeFromView <$> v .: "projectName" <*> v .: "viewName"

instance ToJSON WebEvent where
     toJSON (WEViewChanged m) = object [ "type" .= T.pack "viewChanged"
                                          , "map" .= m
                                          ]
     toJSON (WEProjectContent p) = object [ "type" .= T.pack "projectContent"
                                       , "project" .= p
                                       ]
     toJSON (WEProjectStored pn) = object [ "type" .=  T.pack "projectStored"
                                          , "projectName" .= pn
                                          ]
     toJSON (WEMapStored mn) = object [ "type" .=  T.pack "mapStored"
                                          , "mapName" .= mn
                                          ]
     toJSON (WEMapContent m) = object [ "type" .=  T.pack "mapContent"
                                          , "map" .= m
                                          ]
     toJSON (WESubscribedToView pn vn) = object [ "type" .=  T.pack "subscribedToView"
                                          , "projectName" .= pn
                                          , "viewName" .= pn
                                          ]
     toJSON (WEUnsubscribedFromView pn vn) = object [ "type" .=  T.pack "unsubscribedFromView"
                                          , "projectName" .= pn
                                          , "viewName" .= pn
                                          ]