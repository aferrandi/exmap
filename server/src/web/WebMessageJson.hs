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

