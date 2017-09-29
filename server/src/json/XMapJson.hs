{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module XMapJson where

import Data.Aeson
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import XMapTypes

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
