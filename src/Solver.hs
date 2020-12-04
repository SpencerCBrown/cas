module Solver
    (
        simplify,
        equivalent,
        additiveIdentity
    ) where

import ParseExpr

validate :: Expr -> Maybe Expr
validate _ = undefined



simplify :: Expr -> Expr
simplify = additiveIdentity . multiplicativeIdentity

-- returns true iff the expressions are equivalent, that is, they are identical with simplifications applied.
equivalent :: Expr -> Expr -> Bool
equivalent e1 e2 = simplify e1 == simplify e2

-- x + 0 = x
additiveIdentity (Parens e) = additiveIdentity e
additiveIdentity (Value d) = Value d
additiveIdentity (ID id) = ID id
additiveIdentity (Add e1 e2)
    | e1 == (Value 0)   = additiveIdentity e2
    | e2 == (Value 0)   = additiveIdentity e1
    | otherwise         = Add (additiveIdentity e1) (additiveIdentity e2)
additiveIdentity (Sub e1 e2)
    | e1 == (Value 0)   = additiveIdentity e2
    | e2 == (Value 0)   = additiveIdentity e1
    | otherwise         = Sub (additiveIdentity e1) (additiveIdentity e2)
additiveIdentity (Mult e1 e2) = Mult (additiveIdentity e1) (additiveIdentity e2)
additiveIdentity (Div e1 e2) = Div (additiveIdentity e2) (additiveIdentity e2)
additiveIdentity (FunctionExpr s es) = FunctionExpr s (map additiveIdentity es)

multiplicativeIdentity (Parens e) = multiplicativeIdentity e
multiplicativeIdentity (Value d) = Value d
multiplicativeIdentity (ID id) = ID id
multiplicativeIdentity (Add e1 e2) = Add (multiplicativeIdentity e1) (multiplicativeIdentity e2)
multiplicativeIdentity (Sub e1 e2) = Sub (multiplicativeIdentity e1) (multiplicativeIdentity e2)
multiplicativeIdentity (Mult e1 e2)
    | e1 == (Value 1)   =   multiplicativeIdentity e2
    | e2 == (Value 1)   =   multiplicativeIdentity e1
    | otherwise         =   Mult (multiplicativeIdentity e1) (multiplicativeIdentity e2)
multiplicativeIdentity (Div e1 e2)
    | e1 == (Value 1)   =   Div e1 (multiplicativeIdentity e2) -- 1/x != x
    | e2 == (Value 1)   =   multiplicativeIdentity e1
    | otherwise         =   Div (multiplicativeIdentity e1) (multiplicativeIdentity e2)
multiplicativeIdentity (FunctionExpr s es) = FunctionExpr s (map multiplicativeIdentity es)

-- x * 0 = 0
multiplyByZero (Parens e)   = multiplyByZero e
multiplyByZero (Value d)    = Value d
multiplyByZero (ID id)      = ID id
multiplyByZero (Add e1 e2)  = Add (multiplyByZero e1) (multiplyByZero e2)
multiplyByZero (Sub e1 e2)  = Sub (multiplyByZero e1) (multiplyByZero e2)
multiplyByZero (Mult e1 e2)
    | e1 == (Value 0)       = e1
    | e2 == (Value 0)       = e2
    | otherwise             = Mult (multiplyByZero e1) (multiplyByZero e2)
multiplyByZero (Div e1 e2)  = (Div e1 e2)
multiplyByZero (FunctionExpr s es)  = FunctionExpr s (map multiplicativeIdentity es)

-- x + x = 2*x, a*x + b*x = (a+b)*x
factorVariables (Parens e)  = factorVariables e
factorVariables (Value d)   = Value d
factorVariables (ID id)     = ID id
factorVariables (Add e1