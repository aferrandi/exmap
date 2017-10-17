module FormulaText (formulaToText) where

import qualified Data.Text as T

import Formula
import XMapTypes
import Calculation

formulaToText :: XFormula -> CalculationFormulaText
formulaToText = CalculationFormulaText . formulaToTextRec id

formulaToTextRec :: (T.Text -> T.Text) -> XFormula -> T.Text
formulaToTextRec _ (XFMap (XMapName mn))  = T.intercalate (T.singleton '/') mn
formulaToTextRec t (XFOperation on f1 f2) = t $ T.unwords  [showT on, formulaToTextRec pars f1, formulaToTextRec pars f2]
formulaToTextRec t (XFApplication an f) = t $ T.unwords [showT an, formulaToTextRec pars f]

showT :: Show a => a -> T.Text
showT v = T.pack $ show v

pars :: T.Text -> T.Text
pars t = T.cons '(' $ T.snoc t ')'