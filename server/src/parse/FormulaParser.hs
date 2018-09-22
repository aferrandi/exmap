module FormulaParser (parseFormula) where

import qualified Data.Attoparsec.Text as P
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.Char as C
import Control.Monad (fail)

import Formula
import Operations
import XMapTypes
import TextEnums
import Calculation

parseFormula :: CalculationFormulaText -> Either String XFormula
parseFormula (CalculationFormulaText s) = P.parseOnly (parseFormulaRootNode <* P.endOfInput) $ T.strip s

parseFormulaRootNode :: P.Parser XFormula
parseFormulaRootNode = do
   parseOperation
    <|> parseMap

parseFormulaNode :: P.Parser XFormula
parseFormulaNode = do
    parseOperationInPars
        <|> parseMap

skipSpaceAndChar :: Char -> P.Parser ()
skipSpaceAndChar c = do
    P.skipSpace
    _ <- P.char c
    return ()

skipClosePar :: P.Parser()
skipClosePar = skipSpaceAndChar ')'

skipOpenPar :: P.Parser()
skipOpenPar = skipSpaceAndChar '('


parseMap :: P.Parser XFormula
parseMap = do
    s <- parsePath
    return $ XFMap (XMapName s)

parsePath :: P.Parser [T.Text]
parsePath = do
    P.skipSpace
    P.sepBy1 parseVar (P.char '/')

parseName :: P.Parser T.Text
parseName = do
    P.skipSpace
    parseVar

parseVar :: P.Parser T.Text
parseVar = do
    c <- P.letter
    t <- P.takeWhile isVarLetter
    let s = T.cons c t
--    traceM $ "var: " ++ show s
    return s

isVarLetter :: Char -> Bool
isVarLetter c = C.isAlphaNum c || c == '_'

parseOperationName :: P.Parser OperationName
parseOperationName = do
    s <- parseName
    case enumWithTextCaseInsensitive enumValues s of
        Just n -> return n
        Nothing -> fail $ "not an operation name " ++ T.unpack s

parseOperation :: P.Parser XFormula
parseOperation = do
    op <- parseOperationName
    fs <- P.many' parseFormulaNode
    return $ XFOperation op fs

parseOperationInPars :: P.Parser XFormula
parseOperationInPars = do
    skipOpenPar
    op <- parseOperationName
    fs <- P.many' parseFormulaNode
    skipClosePar
    return $ XFOperation op fs
