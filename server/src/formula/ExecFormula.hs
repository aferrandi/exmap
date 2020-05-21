module ExecFormula (execFormula, formulaResultType) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.List as L

import XMapTypes
import XFunction
import Operations
import Formula
import FormulaText (mapNameToText)
import OperationTypes

type XMapTypeByName = M.Map XMapName XMapType
type XMapTypeErr = Either Error XMapType

execFormula :: XFormula ->  XMapByName -> OperationMode -> XMapErr
execFormula xf mbn om = case xf of
    XFMap n -> case M.lookup n mbn of
        Just m -> Right m
        Nothing -> Left (Error $ T.pack ("map " ++ T.unpack (mapNameToText n) ++ " not found"))
    XFOperation fn subfs -> do
        tms <- mapM (\m -> execFormula m mbn om) subfs
        let f = operationRepository fn
        f om tms


formulaResultType :: XFormula ->  XMapTypeByName -> XMapTypeErr
formulaResultType xf xbn = formulaResultTypeRec xf
      where
         formulaResultTypeRec :: XFormula -> XMapTypeErr
         formulaResultTypeRec xf = case xf of
            XFMap n -> case M.lookup n xbn of
                    Just t -> Right t
                    Nothing -> Left (Error $ T.pack ("map " ++ T.unpack (mapNameToText n) ++ " not found"))
            XFOperation fn subfs -> case M.lookup fn operationTypeByName of
                    Just ot -> functionType fn ot subfs
                    Nothing -> Left (Error $ T.pack ("No formula with name " ++ show fn))
         functionType :: OperationName -> OperationType -> [XFormula] -> XMapTypeErr
         functionType fn ot subfs = case (returnType ot) of
            ParameterDouble -> Right TypeDouble
            ParameterInt -> Right TypeInt
            ParameterText -> Right TypeText
            ParameterBool -> Right TypeBool
            ParameterDate -> Right TypeDate
            ParameterAny -> anyFunctionType fn ot subfs
         anyFunctionType :: OperationName -> OperationType -> [XFormula] -> XMapTypeErr
         anyFunctionType fn ot subfs = case L.find (\(p, s) -> p == ParameterAny) $ L.zip (parametersTypes ot) subfs of
                             Just (p, s) -> formulaResultTypeRec s
                             Nothing -> Left (Error $ T.pack ("No parameter any in the formula " ++ show fn))

