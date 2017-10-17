module FormulaParser (parseFormula) where

import qualified Data.Attoparsec.Text as P
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.Char as C
import Control.Monad (fail)

import Formula
import Operations
import Applications
import XMapTypes
import TextEnums
import Calculation

parseFormula :: CalculationFormulaText -> Either String XFormula
parseFormula (CalculationFormulaText s) = P.parseOnly (parseFormulaRootNode <* P.endOfInput) s

parseFormulaRootNode :: P.Parser XFormula
parseFormulaRootNode = do
   parseOperation
    <|> parseApplication
    <|> parseMap

parseFormulaNode :: P.Parser XFormula
parseFormulaNode = do
    parseOperationInPars
        <|> parseApplicationInPars
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

parseOperationInPars :: P.Parser XFormula
parseOperationInPars = do
    skipOpenPar
    f <- parseOperation
    skipClosePar
    return f

parseApplicationInPars :: P.Parser XFormula
parseApplicationInPars = do
    skipOpenPar
    f <- parseApplication
    skipClosePar
    return f

parseMap :: P.Parser XFormula
parseMap = do
    s <- parsePath
    return $ XFMap (XMapName s)

--splitPath :: T.Text -> [T.Text]
--splitPath = T.split (== '/')

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
    t <- P.takeWhile C.isAlphaNum
    let s = T.cons c t
--    traceM $ "var: " ++ show s
    return s


parseOperationName :: P.Parser OperationName
parseOperationName = do
    s <- parseName
    case enumWithTextCI enumValues s of
        Just n -> return n
        Nothing -> fail $ "not an operation name " ++ T.unpack s


parseApplicationName :: P.Parser ApplicationName
parseApplicationName = do
    s <- parseName
    case enumWithTextCI enumValues s of
        Just n -> return n
        Nothing -> fail $ "not an application name " ++ T.unpack s


parseOperation :: P.Parser XFormula
parseOperation = do
    op <- parseOperationName
    f1 <- parseFormulaNode
    f2 <- parseFormulaNode
    return $ XFOperation op f1 f2

parseApplication :: P.Parser XFormula
parseApplication = do
    ap <- parseApplicationName
    f1 <- parseFormulaNode
    return $ XFApplication ap f1

