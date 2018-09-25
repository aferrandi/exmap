module FormulaText (formulaToText, mapNameToText) where

import qualified Data.Text as T

import Formula
import XMapTypes
import Calculation
import ShowText

formulaToText :: XFormula -> CalculationFormulaText
formulaToText = CalculationFormulaText . formulaToTextRec id

formulaToTextRec :: (T.Text -> T.Text) -> XFormula -> T.Text
formulaToTextRec _ (XFMap mn) = mapNameToText mn
formulaToTextRec t (XFOperation on fs) = t $ T.unwords $ showT on : map (formulaToTextRec pars) fs

mapNameToText :: XMapName -> T.Text
mapNameToText (XMapName mn) = T.intercalate (T.singleton '/') mn

pars :: T.Text -> T.Text
pars t = T.cons '(' $ T.snoc t ')'