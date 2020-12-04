module ParseExpr
    ( Equation (..),
      Expr (..),
      printExpr,
      runMyParser,
      runMyParser',
      printEq
    ) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Number

--- Each parse input should be a single line containing a nested mathematical expression

upcast :: Either Integer Double -> Double
upcast (Right d) = d
upcast (Left i) = fromIntegral i

--- We need to parse individual numbers
parsenum = parsecMap upcast
  (try (decFract True)
    <|> try (decFract False))

--- We also need to parse string indentifiers. We can match/filter special names later.
parseId = many letter

--- parse functions/symbols/etc
parseSymbol = do
  char '\\'
  parseId

strToSymbol = parsecMap (\str -> strToSymbol' str)
strToSymbol' str
  | str == "sin"  = SIN
  | str == "cos"  = COS
  | str == "tan"  = TAN
  | str == "exp"  = EXP
  | str == "log"   = LOG

-- highest operator precedence
-- this should return type Expr (with constructors )

-- for now, just supporting 'functions' with one inner expression.
-- will need to extend for functions like exp(base, exponent)
-- currently, assume the expression is the exponent and the base is always e.
parseFunctionExpr = do
  functionName <- strToSymbol parseSymbol
  char '('
  innerExprs <- parseAddExpr `sepBy` (char ',')
  char ')'
  return (FunctionExpr functionName innerExprs)


parseParenthesizedExpr = do
  char '('
  e <- parseAddExpr
  char ')'
  return e

parsePrimary = do
  try parseFunctionExpr
  <|> parsecMap Parens (try parseParenthesizedExpr)
  <|> (parsecMap Value (try parsenum))
  <|> (parsecMap ID (try parseId))

parseMultExpr = do
  leftOp <- parsePrimary
  rightOp <- optionMaybe (parseMultExpr')
  return (constructMultExpr leftOp rightOp)

constructMultExpr :: Expr -> Maybe (Char, Expr) -> Expr
constructMultExpr e Nothing = e
constructMultExpr e (Just (operator, rightExpr)) = if operator == '*' then Mult e rightExpr else Div e rightExpr

parseMultExpr' = do
  operator <- oneOf "*/"
  rightOp <- parseMultExpr
  return (operator, rightOp)

parseAddExpr = do
  leftExpr <- parseMultExpr
  opAndRightExpr <- optionMaybe (parseAddExpr')
  return (constructAddExpr leftExpr opAndRightExpr)

constructAddExpr e Nothing = e
constructAddExpr e (Just (operator, rightExpr)) = if operator == '+' then Add e rightExpr else Sub e rightExpr

parseAddExpr' = do
  operator <- oneOf "+-"
  rightExpr <- parseAddExpr
  return (operator, rightExpr)

parseEquation = do
  expr1 <- parseAddExpr
  char '='
  expr2 <- parseAddExpr
  return (Equation expr1 expr2)

runMyParser :: [Char] -> Either ParseError Equation
runMyParser input = parse parseEquation "" $ filter (\s -> s /= ' ') input

runMyParser' :: [Char] -> Either ParseError Expr
runMyParser' input = parse parseAddExpr "" $ filter (\s -> s /= ' ') input

data Symbol = SIN | COS | TAN | EXP | LOG
  deriving (Show, Eq)

data Equation = Equation Expr Expr

data Expr = 
  Parens Expr -- parenthesized expression
    | FunctionExpr Symbol [Expr]
    | Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr
    | Div Expr Expr
    | Value Double
    | ID [Char]
  deriving Eq

instance Ord Expr where
  (Value d1) `compare` (Value d2) = d1 `compare` d2
  (ID s1) `compare` (ID s2) = s1 `compare` s2
  Add 

printExpr :: Expr -> String
printExpr expr = "(" ++ (printExpr' expr) ++ ")"
printExpr' (Value d) = (show d)
printExpr' (ID id) = (show id)
printExpr' (Add e1 e2) = "ADD " ++ "(" ++ (printExpr' e1) ++ ")" ++ " " ++ "(" ++ (printExpr' e2) ++ ")"
printExpr' (Sub e1 e2) = "SUB " ++ "(" ++ (printExpr' e1) ++ ")" ++ " " ++ "(" ++ (printExpr' e2) ++ ")"
printExpr' (Mult e1 e2) = "MULT " ++ "(" ++ (printExpr' e1) ++ ")" ++ " " ++ "(" ++ (printExpr' e2) ++ ")"
printExpr' (Div e1 e2) = "DIV " ++ "(" ++ (printExpr' e1) ++ ")" ++ " " ++ "(" ++ (printExpr' e2) ++ ")"
printExpr' (Parens e) = printExpr' e
printExpr' (FunctionExpr name es) = (show name) ++ " (" ++ (printFunctionExprs es) ++ ")"

printFunctionExprs :: [Expr] -> String
printFunctionExprs es = foldl (\accum element -> if accum == "" then printExpr' element else accum ++ "," ++ printExpr' element) "" es

printEq :: Equation -> String
printEq (Equation e1 e2) = "(" ++ "=" ++ " " ++ (printExpr e1) ++ " " ++ (printExpr e2) ++ " " ++ ")"