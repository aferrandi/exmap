{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module ViewJson where

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

instance FromJSON ViewRowHeaderType where
   parseJSON (String v) = readT <$> pure v
   parseJSON _ = mempty

instance ToJSON ViewRowHeaderType where
   toJSON v = String $ showT v

instance FromJSON ViewRow where
   parseJSON (Object v) = ViewRow <$> v .: "items" <*> v .: "headerType"
   parseJSON _ = mempty

instance ToJSON ViewRow where
    toJSON (ViewRow is id) = object [ "items" .= is
                                    , "headerType" .= id
                                    ]

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

