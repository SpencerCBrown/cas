module Solve
    (
        solutionSteps,
        simplifyEquation,
        printExpr,
        printEquation
    ) where

import ParseExpr
import Simplify

import Data.List
import Data.List.Ordered
import Debug.Trace

simplifyEquation (Equation left right) = Equation (simplifyExpr left) (simplifyExpr right)

-- checks that a simplified expression only has a single variable
univariate :: BAE -> Bool
univariate expr = undefined

{- checks that a simplified expression does not have a variable raised to a power >= 2
    as we do not solve quadratic equations (or any polynomials of degree >= 2) -}
degreeOne :: BAE -> Bool
degreeOne expr = undefined

symbolsInExpr :: BAE -> [Symbol]
symbolsInExpr (BAEInteger _) = []
symbolsInExpr (BAEFraction _) = []
symbolsInExpr (BAESymbol x) = [x]
symbolsInExpr (BAEProduct os) = concatMap symbolsInExpr os
symbolsInExpr (BAESum os) = concatMap symbolsInExpr os
symbolsInExpr (BAEQuotient p q) = symbolsInExpr p ++ symbolsInExpr q
symbolsInExpr (BAEUnaryExpr c e) = symbolsInExpr e
symbolsInExpr (BAEBinaryDiff a b) = symbolsInExpr a ++ symbolsInExpr b
symbolsInExpr (BAEPower a b) = symbolsInExpr a ++ symbolsInExpr b
symbolsInExpr (BAEFunction s e) = symbolsInExpr e
symbolsInExpr BAEUndefined = []


subtractExpr :: BAE -> BAE -> BAE
-- subtract a from b
subtractExpr a b = simplifyExpr (BAEBinaryDiff b a)

addExpr :: BAE -> BAE -> BAE
addExpr a b = simplifyExpr (BAESum [a,b])

multiplyExpr :: BAE -> BAE -> BAE
multiplyExpr a b = simplifyExpr (BAEProduct [a, b])

multInverse :: BAE -> BAE
multInverse expr = simplifyExpr (BAEPower expr (BAEInteger (-1)))

addInverse :: BAE -> BAE
addInverse expr = simplifyExpr (BAEProduct [BAEInteger (-1), expr])

swap :: BAE -> BAE -> BAE -> BAE
-- swaps operands in a really weird way, to fit in with my existing paradigm of applying operation steps to the equation. Ugly.
swap a b c = if c == a then b else a

undefine :: BAE -> BAE
undefine _ = BAEUndefined

identity :: BAE -> BAE
identity a = a


solutionSteps :: Equation -> (String, Equation)
solutionSteps (Equation left right) = let
    -- subtract the right side from both sides. Now in the form: f(x) + G = 0. Only do this if there is a variable in the right side.
    left' = simplifyExpr (subtractExpr right left)
    right' = simplifyExpr (subtractExpr right right)
    thisEquation = if not (null (symbolsInExpr right)) then Equation (simplifyExpr left') (BAEInteger 0) else Equation left right 
    thisStep = if not (null (symbolsInExpr right)) then "Subtract " ++ printExpr right ++ " from both sides and simplify.\n\n" ++ printEquation thisEquation ++ "\n\n" else "Simplify.\n\n" ++ printEquation thisEquation

    (followingSteps, finalEquation) = solutionSteps' thisEquation
    in (thisStep ++ followingSteps, finalEquation)

-- this function recursively solves the equation step-by-step, simplifying each time.
solutionSteps' :: Equation -> (String, Equation)
solutionSteps' (Equation left right) = let
    (thisStep, thisOp) = case left of
        BAESymbol x -> ("Solution: ", identity)
        BAEProduct (c:fx:[])  -> ("Multiply both sides by " ++ printExpr (multInverse c) ++ " and simplify.\n\n", multiplyExpr (multInverse c))
        BAESum (c:fx:[]) -> ("Add " ++ printExpr (addInverse c) ++ " to both sides and simplify.\n\n", addExpr (addInverse c))
        BAEPower base expon -> if expon == (BAEInteger (-1)) then ("Multiply both sides by " ++ printExpr prod ++ " and simplify.\n\n", op) else ("", undefine) where
            mleft = multInverse left
            mright = multInverse right
            prod = simplifyExpr (BAEProduct [mleft, mright])
            leftInv = multiplyExpr (mleft)
            rightInv = multiplyExpr (mright)
            op = swap (rightInv (leftInv left)) (rightInv (leftInv right)) . rightInv . leftInv
        _ -> error ("Left:  " ++ show left ++ "\n" ++ "Step:   " ++ "thisStep" ++ "\n")

    thisEquation = Equation (simplifyExpr (thisOp left)) (simplifyExpr (thisOp right))
    thisMsg = thisStep ++ printEquation thisEquation ++ "\n\n"
    (followingSteps, finalEquation) = solutionSteps' thisEquation
    in if isIsolated left && isDefined right then (thisMsg, thisEquation) else if eqIsUndefined thisEquation then ("Cannot be solved", Equation BAEUndefined BAEUndefined) else (thisMsg ++ followingSteps, finalEquation)

isIsolated (BAESymbol _) = True
isIsolated _ = False

isDefined (BAEUndefined) = False
isDefined _ = True

eqIsUndefined (Equation left right) = not (isDefined left && isDefined right)

printExpr (BAESymbol x) = x
printExpr (BAEInteger i) = if i < 0 then "(" ++ show i ++ ")" else show i
printExpr (BAEFraction d) = if d < 0 then "(" ++ show d ++ ")" else show d
printExpr (BAEProduct os) = intercalate "*" $ map printProduct os
printExpr (BAESum os) = intercalate "+" $ map printSum os
printExpr (BAEPower base expon) = printExpr base ++ "^" ++ "(" ++ printExpr expon ++ ")"
printExpr (BAEFunction sym expr) = sym ++ "(" ++ printExpr expr ++ ")"
printExpr BAEUndefined = "undefined"

printProduct expr = case expr of
    (BAESum os) ->  "(" ++ printExpr expr ++ ")"
    (BAEProduct os) -> "(" ++ printExpr expr ++ ")"
    expr -> printExpr expr

printSum expr = case expr of
    (BAESum os) -> "(" ++ printExpr expr ++ ")"
    expr -> printExpr expr

printEquation (Equation left right) = printExpr left ++ " = " ++ printExpr right