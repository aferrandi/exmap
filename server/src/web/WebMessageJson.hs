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
      Just (String "loadProject") ->  WLoadProject <$> v .: "projectName"
      Just (String "storeProject") -> WStoreProject <$> v .: "project"
      Just (String "loadMap") ->  WLoadMap <$> v .: "projectName" <*> v .: "mapName"
      Just (String "storeMap") ->  WStoreMap <$> v .: "projectName" <*> v .: "map"
      Just (String "subscribeToView") -> WSubscribeToView <$> v .: "projectName" <*> v .: "viewName"
      Just (String "unSubscribeFromView") ->  WUnSubscribeFromView <$> v .: "projectName" <*> v .: "viewName"

