{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module ParseExpr
    (
       Symbol,
       BAE (..),
       Equation (..),
       parseExpr,
       parseEquation
    ) where


import Generics.Deriving.Base(Generic)
import Generics.Deriving.Eq(GEq, geq)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Number

-- type definition for Basic Algebraic Expression
data BAE =
    BAEInteger Integer
  | BAEFraction Double
  | BAESymbol Symbol
  | BAEProduct [BAE]
  | BAESum [BAE]
  | BAEQuotient BAE BAE
  | BAEUnaryExpr Char BAE -- for e.g. unary minus the Char will be '-'.
  | BAEBinaryDiff BAE BAE
  | BAEPower BAE BAE
  | BAEFunction Symbol BAE
  | BAEUndefined -- should this be represented as (BAESymbol "undefined") ?
  deriving (Show, Generic)

instance GEq BAE

instance Eq BAE where
  BAEInteger i == BAEFraction f = fromIntegral i == f
  BAEFraction f == BAEInteger i = fromIntegral i == f
  x == y = geq x y

data Equation = Equation BAE BAE
  deriving Show

type Symbol = String

parseSymbol = do
  optional (char '\\') -- optionally, parse the LaTeX backslash at the start of a predefined symbol. We ignore this.
  letters <- many1 letter
  return (BAESymbol letters)

parseNumber = parsecMap (\s -> case s of 
  Right f -> BAEFraction f
  Left i -> BAEInteger i) decimalFract

parseFunction = do
  (BAESymbol sym) <- parseSymbol
  char '('
  expr <- parseBAE
  char ')'
  return (BAEFunction sym expr)

parseParenthesizedExpr = do
  char '('
  e <- parseBAE
  char ')'
  return e

parseQuotient = do
  char '\\'
  string "frac"
  char '{'
  numerator <- parseBAE
  char '}'
  char '{'
  denominator <- parseBAE
  char '}'
  return (BAEQuotient numerator denominator)

parsePrimary = (try parseFunction)
  <|> (try parseQuotient)
  <|> (try parseSymbol)
  <|> (try parseNumber)
  <|> (try parseParenthesizedExpr)

parsePower = do
  base <- parsePrimary
  opAndExpn <- optionMaybe (char '^' >> parsePower)
  return (case opAndExpn of
    Nothing     -> base
    Just expon  -> BAEPower base expon)

parseUnary = do
  unaryOp <- optionMaybe ((try (char '-')) <|> (try (char '+'))) -- unary BAE must support unary product internally, but we don't have to parse for it.
  expr <- parsePower
  return (case unaryOp of 
    Nothing   -> expr
    Just c    -> BAEUnaryExpr c expr)

parseProduct = do
  -- sequence of parseUnary and parseBAE?
  leftExpr <- parseUnary
  rightExprs <- optionMaybe (char '*' >> (parseUnary `sepBy` (char '*')))
  return (case rightExprs of
    Nothing   -> leftExpr
    Just es   -> BAEProduct (leftExpr:es))

parseBinaryDiff = do
  leftExpr <- parseProduct
  rightExpr <- optionMaybe (char '-' >> parseProduct)
  return (case rightExpr of
    Nothing   -> leftExpr
    Just r    -> BAEBinaryDiff leftExpr r)

parseSum = do
  leftExpr <- parseBinaryDiff
  rightExprs <- optionMaybe (char '+' >> ((parseBinaryDiff) `sepBy` (char '+')))
  return (case rightExprs of
    Nothing   -> leftExpr
    Just es   -> BAESum (leftExpr:es))

parseBAE = parseSum

parseEq = do
  left <- parseBAE
  char '='
  right <- parseBAE
  return (Equation left right)

parseExpr input = parse parseBAE "" $ filter (/= ' ') input

parseEquation input = parse parseEq "" $ filter (/= ' ') input