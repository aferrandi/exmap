module FormulaParser (parseFormula) where

import qualified Data.Attoparsec.Text as P
import Control.Applicative ((<|>))
import qualified Data.Text as T (Text, pack, unpack, split)
import qualified Data.Char as C
import Control.Monad (fail)
import Text.Read (readEither)
import Debug.Trace

import Formula
import Operations
import Applications
import XMapTypes
import TextEnums

parseFormula :: String -> Either String XFormula
parseFormula s = P.parseOnly (parseFormulaRootNode <* P.endOfInput) (T.pack s)

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
    P.char c
    return ()

skipClosePar = skipSpaceAndChar ')'
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

splitPath :: T.Text -> [T.Text]
splitPath = T.split (== '/')

parsePath :: P.Parser [String]
parsePath = do
    P.skipSpace
    P.sepBy1 parseVar (P.char '/')

parseName :: P.Parser String
parseName = do
    P.skipSpace
    parseVar

parseVar :: P.Parser String
parseVar = do
    c <- P.letter
    t <- P.takeWhile C.isAlphaNum
    let s = c:T.unpack t
--    traceM $ "var: " ++ show s
    return s


parseOperationName :: P.Parser OperationName
parseOperationName = do
    s <- parseName
    case enumWithTextCI enumValues s of
        Just n -> return n
        Nothing -> fail $ "not an operation name " ++ s


parseApplicationName :: P.Parser ApplicationName
parseApplicationName = do
    s <- parseName
    case enumWithTextCI enumValues s of
        Just n -> return n
        Nothing -> fail $ "not an application name " ++ s


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

