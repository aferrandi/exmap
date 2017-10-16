module EncodeCalculation exposing (..)

import Json.Encode exposing (..)
import String exposing (join)
import Dict exposing (..)

import EncodeXMap exposing (..)
import Calculation exposing (..)


encodeCalculationSource : CalculationSource -> Value
encodeCalculationSource cs = object
                 [ ("calculationName", string cs.calculationName)
                 , ("resultName", encodeXmapName cs.resultName)
                 , ("formulaText", string cs.formulaText)
                 , ("operationMode", string (toString cs.operationMode))
                  ]