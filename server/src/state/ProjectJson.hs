{-# LANGUAGE OverloadedStrings   #-}

module ProjectJson where

import Data.Aeson
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.Text as T

import Project
import XMapTypes
import Formula
import XFunction
import TextEnums
import OperationTypes
import ApplicationTypes


{-
instance FromJSON CalculationName where
 parseJSON (Object v) =
    Person <$> v .: "firstName"
           <*> v .: "lastName"
           <*> v .: "age"
           <*> v .: "likesPizza"
 parseJSON _ = mzero

instance ToJSON Person where
 toJSON (Person firstName lastName age likesPizza) =
    object [ "firstName"  .= firstName
           , "lastName"   .= lastName
           , "age"        .= age
           , "likesPizza" .= likesPizza
             ]
-}


instance FromJSON XMapName where
   parseJSON (String v) = return $ XMapName (T.splitOn "/" v)

instance ToJSON XMapName where
   toJSON (XMapName vs) = String (T.intercalate "/" vs)

instance FromJSON XFormula where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "map") -> XFMap <$> v .: "name"
      Just (String "operation") ->  XFOperation <$> v .: "name" <*> v .: "formula1" <*> v .: "formula2"
      Just (String "application") ->  XFApplication <$> v .: "name" <*> v .: "formula"

instance ToJSON XFormula where
     toJSON (XFMap n) = object [ "type"  .= T.pack "map"
                              , "name"   .= n
                              ]
     toJSON (XFOperation n f1 f2) = object [ "type"  .=  T.pack "operation"
                                            , "name"   .= n
                                            , "formula1" .= f1
                                            , "formula2" .= f2
                                            ]
     toJSON (XFApplication n f) = object [ "type"  .=  T.pack "application"
                                            , "name"   .= n
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
   toJSON (CalculationName v) = String $ showT v


instance FromJSON Calculation  where
   parseJSON (Object v) =
      Calculation  <$> v .: "calculationName"
             <*> v .: "formula"
             <*> v .: "maps"
             <*> v .: "operationMode"

instance ToJSON Calculation  where
     toJSON (Calculation calculationName formula maps operationMode) =
        object [ "calculationName"  .= calculationName
               , "formula"   .= formula
               , "maps"        .= maps
               , "operationMode" .= operationMode
                 ]

