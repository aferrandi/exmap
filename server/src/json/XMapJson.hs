{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module XMapJson where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.Text as T
import ShowText

import XMapTypes

instance FromJSON XMapName where
   parseJSON (String v) = return $ XMapName (T.splitOn "/" v)

instance ToJSON XMapName where
   toJSON (XMapName vs) = String (T.intercalate "/" vs)

instance FromJSON XMapKey where
   parseJSON (String v) = return $ XMapKey v

instance ToJSON XMapKey where
   toJSON (XMapKey v) = String v

instance ToJSONKey XMapKey where
    toJSONKey = toJSONKeyText toText
        where toText (XMapKey v) = v

instance FromJSONKey XMapKey where
    fromJSONKey = FromJSONKeyText XMapKey

instance ToJSON XMapType where
   toJSON v = String $ case v of
        TypeDouble -> "double"
        TypeInt -> "int"
        TypeText -> "string"
        TypeBool -> "bool"

instance FromJSON XMap where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "double") -> XMapDouble <$> v .: "values"
      Just (String "int") ->  XMapInt <$> v .: "values"
      Just (String "string") ->  XMapString <$> v .: "values"
      Just (String "bool") ->  XMapBool <$> v .: "values"

instance ToJSON XMap where
     toJSON (XMapDouble values) = object [ "type" .= T.pack "double"
                                          , "values" .= values
                                          ]
     toJSON (XMapInt values) = object [ "type" .= T.pack "int"
                                       , "values" .= values
                                       ]
     toJSON (XMapString values) = object [ "type" .=  T.pack "string"
                                          , "values" .= values
                                          ]
     toJSON (XMapBool values) = object [ "type" .=  T.pack "bool"
                                          , "values" .= values
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
