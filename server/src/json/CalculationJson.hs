{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module CalculationJson where

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

instance ToJSON ParameterType where
   toJSON v = String $ case v of
        ParameterDouble -> "double"
        ParameterInt -> "int"
        ParameterText -> "string"
        ParameterBool -> "bool"
        ParameterDate -> "date"        
        ParameterAny -> "any"

instance FromJSON XFormula where
   parseJSON (Object v) = case HML.lookup "type" v of
      Just (String "map") -> XFMap <$> v .: "name"
      Just (String "operation") ->  XFOperation <$> v .: "name" <*> v .: "parameters"
      _ -> mempty
   parseJSON _ = mempty

instance ToJSON XFormula where
     toJSON (XFMap n) = object [ "type"  .= T.pack "map"
                              , "name"   .= n
                              ]
     toJSON (XFOperation n fs) = object [ "type" .=  T.pack "operation"
                                            , "name" .= n
                                            , "parameters" .= fs
                                            ]
instance FromJSON OperationMode where
   parseJSON (String v) = readT <$> pure v
   parseJSON _ = mempty

instance ToJSON OperationMode where
   toJSON v = String $ showT v


instance FromJSON OperationCategory where
   parseJSON (String v) = readT <$> pure v
   parseJSON _ = mempty

instance ToJSON OperationCategory where
   toJSON v = String $ showT v

instance FromJSON OperationName where
   parseJSON (String v) = readT <$> pure v
   parseJSON _ = mempty

instance ToJSON OperationName where
   toJSON v = String $ showT v

instance FromJSON CalculationName where
   parseJSON (String v) = return $ CalculationName v

instance ToJSON CalculationName where
   toJSON (CalculationName v) = String v

instance FromJSON Calculation  where
   parseJSON (Object v) =
      Calculation  <$> v .: "calculationName"
             <*> v .: "resultName"
             <*> v .: "formula"
             <*> v .: "operationMode"
   parseJSON _ = mempty

instance ToJSON Calculation  where
     toJSON (Calculation calculationName resultName formula operationMode) =
        object [ "calculationName" .= calculationName
               , "resultName" .= resultName
               , "formula" .= formula
               , "operationMode" .= operationMode
                 ]

instance FromJSON CalculationFormulaText where
   parseJSON (String v) = return $ CalculationFormulaText v

instance ToJSON CalculationFormulaText where
   toJSON (CalculationFormulaText v) = String v

instance FromJSON CalculationSource  where
   parseJSON (Object v) =
      CalculationSource <$> v .: "calculationName"
             <*> v .: "resultName"
             <*> v .: "formulaText"
             <*> v .: "operationMode"
   parseJSON _ = mempty

instance ToJSON CalculationSource where
     toJSON (CalculationSource calculationName resultName formulaText operationMode) =
        object [ "calculationName" .= calculationName
               , "resultName" .= resultName
               , "formulaText" .= formulaText
               , "operationMode" .= operationMode
                 ]

instance ToJSON OperationId where
     toJSON (OperationId category name) =
        object [ "category" .= category
               , "name" .= name
                 ]

instance ToJSON OperationType where
     toJSON (OperationType operationId parametersTypes returnType) =
        object [ "operationId" .= operationId
               , "parametersTypes" .= parametersTypes
               , "returnType" .= returnType
                 ]

instance ToJSON Functions where
     toJSON (Functions operationTypes) =
        object [ "operationTypes" .= operationTypes ]
