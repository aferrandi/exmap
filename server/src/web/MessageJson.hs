{-# LANGUAGE OverloadedStrings   #-}
module MessageJson where

import Data.Aeson
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import ProjectJson
import Messages

instance FromJSON Request where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "loadProject") ->  LoadProject <$> v .: "projectName"
      Just (String "storeProject") -> StoreProject <$> v .: "project"
      Just (String "loadMap") ->  LoadMap <$> v .: "projectName" <*> v .: "mapName"
      Just (String "storeMap") ->  StoreMap <$> v .: "projectName" <*> v .: "map"
      Just (String "subscribeToView") -> SubscribeToView <$> v .: "projectName" <*> v .: "viewName"
      Just (String "unSubscribeFromView") ->  UnSubscribeFromView <$> v .: "projectName" <*> v .: "viewName"

