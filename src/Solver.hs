{-# LANGUAGE LambdaCase #-}
module Solver
    (
    ) where

import ParseExpr
import Data.List.Ordered

{- unfortunately we can't encode ASAE into the type system.
    That would requirement dependent types, as in coq, agda, or idris
-}

-- convenience functions
isInteger (BAEInteger _) = True
isInteger _ = False

getInteger (BAEInteger i) = i
getInteger _ = undefined

isFraction (BAEFraction _) = True
isFraction _ = False

getFraction (BAEFraction f) = f
getFraction _ = undefined

isPower (BAEPower _ _) = True
isPower _ = False

isUnary (BAEUnaryExpr _ _) = True
isUnary _ = False

isProduct (BAEProduct _) = True
isProduct _ = False

isSum (BAESum _) = True
isSum _ = False

isDiff (BAEBinaryDiff _ _) = True
isDiff _ = False

isQuotient (BAEQuotient _ _) = True
isQuotient _ = False

isPositiveInteger (BAEInteger i) = i > 0
isPositiveInteger _ = False

isPositiveFraction (BAEFraction f) = f > 0
isPositiveFraction _ = False

isUndefined BAEUndefined = True
isUndefined _ = False

simplifyExpr expr
    | isPower expr  = let (BAEPower base expon) = expr in simplifyPower (BAEPower (simplifyExpr base) (simplifyExpr expon))
    | otherwise     = expr

-- simplification transformations
-- TODO: these comparisons to == BAEInteger _ may be weird for non BAEInteger types. Is that OK?
simplifyPower (BAEPower base expon)
    | isUndefined base || isUndefined expon = BAEUndefined
    | base == BAEInteger 0 = if (isPositiveInteger expon) || (isPositiveFraction expon) then BAEInteger 0 else BAEUndefined
    | base == BAEInteger 1 = BAEInteger 1
    | isInteger expon   = simplifyIntegerPower (BAEPower base expon)
    | otherwise = (BAEPower base expon)

-- if this is called, base /= 0, expon is an integer
simplifyIntegerPower (BAEPower base expon)
    | isInteger base || isFraction base = simplifyRNE (BAEPower base expon)
    | expon == BAEInteger 0    = BAEInteger 1
    | expon == BAEInteger 1    = base
    | isPower base      = simplifyPowerOfPower base expon
    | isProduct base    = let 
        (BAEProduct os) = base
        newoperands = map (\o -> simplifyIntegerPower (BAEPower o expon)) os
        in simplifyProduct (BAEProduct newoperands)
    | otherwise {- symbol, sum, or function -}  = BAEPower base expon 
    where
        simplifyPowerOfPower :: BAE -> BAE -> BAE
        simplifyPowerOfPower (BAEPower r s) n = if isInteger p then simplifyIntegerPower (BAEPower r p) else BAEPower r p
            where p = simplifyProduct (BAEProduct [s, n])

simplifyProduct a = a

simplifyRNE expr = let 
    v = simplifyRNE' expr
    in if isUndefined v then BAEUndefined else simplifyRationalNumber v

simplifyRNE' :: BAE -> BAE
simplifyRNE' expr
    | isInteger expr    = expr
    | isFraction expr   = expr
    | isUnary expr      = let (BAEUnaryExpr c e) = expr in if c == '-' then evaluateProduct (BAEInteger (-1)) (simplifyRNE' e) else simplifyRNE' e
    | isSum expr || isProduct expr || isDiff expr || isQuotient expr    = case expr of
        (BAESum os) -> if any isUndefined simplifiedOps then BAEUndefined else evaluateSum (simplifiedOps!!0) (simplifiedOps!!1) where simplifiedOps = map simplifyRNE' os
        (BAEProduct os) -> if any isUndefined simplifiedOps then BAEUndefined else evaluateProduct (simplifiedOps!!0) (simplifiedOps!!1) where simplifiedOps = map simplifyRNE' os
        (BAEBinaryDiff a b) -> if isUndefined a' || isUndefined b' then BAEUndefined else evaluateDifference a' b' where
            a' = simplifyRNE' a
            b' = simplifyRNE' b
        (BAEQuotient a b) -> if isUndefined a' || isUndefined b' then BAEUndefined else evaluateQuotient a' b' where
            a' = simplifyRNE' a
            b' = simplifyRNE' b
    | isPower expr      = let (BAEPower base expon) = expr in let base' = simplifyRNE' base in if base' == BAEUndefined then BAEUndefined else evaluatePower base' expon

-- must be fraction or integer
simplifyRationalNumber expr = case expr of
    (BAEInteger i) -> expr
    (BAEFraction f) -> expr -- a double is already in standard form, no need to reduce
    _ -> undefined

evaluateProduct a b = let
    a' = getFraction (intToFrac a)
    b' = getFraction (intToFrac b)
    product = a' * b'
    in if floor product == ceiling product then BAEInteger (truncate product) else BAEFraction product

evaluateSum a b = let
    a' = getFraction (intToFrac a)
    b' = getFraction (intToFrac b)
    sum = a' + b'
    in if floor sum == ceiling sum then BAEInteger (truncate sum) else BAEFraction sum

evaluateDifference a b = let
    a' = getFraction (intToFrac a)
    b' = getFraction (intToFrac b)
    diff = a' - b'
    in if floor diff == ceiling diff then BAEInteger (truncate diff) else BAEFraction diff

evaluateQuotient :: BAE -> BAE -> BAE
evaluateQuotient numerator denominator = if denominator == (BAEInteger 0) then BAEUndefined else BAEFraction (num / denom) where
    (BAEFraction num) = intToFrac numerator
    (BAEFraction denom) = intToFrac denominator

intToFrac intOrFrac = case intOrFrac of
    (BAEInteger x) -> BAEFraction (fromIntegral x)
    (BAEFraction x) -> BAEFraction x
    _ -> undefined

evaluatePower base expon
    | base == BAEInteger 0      = if expon >= BAEInteger 1 then BAEInteger 0 else {- n<=0 -} BAEUndefined
    | otherwise                 = if expon == BAEInteger 0 then BAEInteger 1 else BAEFraction (getFraction (intToFrac base) ^^ getInteger expon)

-- ordering relation for expressions
instance Ord BAE where
    (BAEInteger i1) `compare` (BAEInteger i2)   = i1 `compare` i2
    (BAEFraction f1) `compare` (BAEFraction f2) = f1 `compare` f2
    (BAEInteger i) `compare` (BAEFraction f)    = fromIntegral i `compare` f
    (BAEFraction f) `compare` (BAEInteger i)    = f `compare` fromIntegral i
    (BAESymbol s1) `compare` (BAESymbol s2)     = s1 `compare` s2
    (BAESum operands1) `compare` (BAESum operands2) = let
        cond1 = last operands1 /= last operands2
        recursiveCompare o1 o2 len1 len2 
            | null o1 || null o2    = len1 `compare` len2 
            | cond1                 = last operands1 `compare` last operands2
            | otherwise = recursiveCompare (init o1) (init o2) len1 len2
        --in if cond1 then ord1 else if cond2 then cond2 else if cond3 then cond3
        in recursiveCompare operands1 operands2 (length operands1) (length operands2)
    (BAEProduct operands1) `compare` (BAEProduct operands2) = let
        cond1 = last operands1 /= last operands2
        recursiveCompare o1 o2 len1 len2 
            | null o1 || null o2    = len1 `compare` len2 
            | cond1                 = last operands1 `compare` last operands2
            | otherwise = recursiveCompare (init o1) (init o2) len1 len2
        in recursiveCompare operands1 operands2 (length operands1) (length operands2)
    (BAEPower base1 expon1) `compare` (BAEPower base2 expon2)
        | base1 /= base2    = base1 `compare` base2
        | otherwise         = expon1 `compare` expon2
    (BAEFunction sym1 expr1) `compare` (BAEFunction sym2 expr2) -- so far I've only allowed functions with one operand, so no need to compare all operands as with sums and products
        | sym1 /= sym2  = sym1 `compare` sym2
        | otherwise     = expr1 `compare` expr2
    
    -- mismatched operands
    -- distasteful as it is, the order of these declarations is important. These _ pattern matches need to come after the others.
    (BAEInteger _) `compare` _ = LT
    _ `compare` (BAEInteger _) = GT

    (BAEFraction _) `compare` _ = LT
    _ `compare` (BAEFraction _) = GT

    (BAEProduct _) `compare` _ = LT
    _ `compare` (BAEProduct _) = GT

    (BAEPower _ _) `compare` _ = LT
    _ `compare` (BAEPower _ _) = GT

    (BAESum _) `compare` _ = LT
    _ `compare` (BAESum _) = GT

    (BAEFunction sym1 _) `compare` (BAESymbol sym2)
        | sym1 == sym2   = GT
        | otherwise     = sym1 `compare` sym2

    (BAESymbol sym1) `compare` (BAEFunction sym2 _)
        | sym1 == sym2   = LT
        | otherwise     = sym1 `compare` sym2

isASAE :: BAE -> Bool
isASAE = undefined
-- below are ASAE rules

{- An expression u is an Automatically Simplified Algebraic Expression ASEA
    if it satisfies one of the following rules: -}
-- ASAE-1. u is an integer.
asea_1 (BAEInteger _)   = True
asea_1 _                = False

-- ASAE-2. u is a fraction in standard form.
asea_2 (BAEFraction _)  = True -- BAEFraction contains a double, which is in standard form
asea_2 _                = False

-- ASAE-3. u is a symbol, except the symbol undefined
asea_3 (BAESymbol _)    = True
asea_3 _                = False

{- ASAE-4. u is a product with two or more operands u_1, u_2, ..., u_n, 
    that satisfy the following properties:

    1. Each operand u_i is an ASAE which can be either an integer (/= 0, 1), fraction, 
    symbol (/= undefined), sum, power, or function. 
    An operand of a product must not be a product.

    2. At most one operand u_i is a constant (integer or fraction).

    3. If i /= j, base(u_i) /= base(u_j).

    4. If i < j, then u_i <' u_j.

    where <' is the expression ordering relation.
-}

asae_4 (BAEProduct operands) = let
    operandsAreASAE = all isASAE operands
    noOperandIsProduct = foldl (\accum e -> case e of
        (BAEProduct _) -> False
        _ -> accum) True operands
    atMostOneOperandIsConst = length (filter (\case 
        (BAEInteger _) -> True
        (BAEFraction _) -> True
        _ -> False) operands) <= 1
    in operandsAreASAE && noOperandIsProduct && atMostOneOperandIsConst && isSorted operands

asae_5 (BAESum operands) = let
    operandsAreASAE = all isASAE operands
    noOperandIsSum = foldl (\accum e -> case e of
        (BAESum _) -> False
        _ -> accum) True operands
    atMostOneOperandIsConst = length (filter (\case 
        (BAEInteger _) -> True
        (BAEFraction _) -> True
        _ -> False) operands) <= 1
    in operandsAreASAE && noOperandIsSum && atMostOneOperandIsConst && isSorted operands

asae_6 (BAEPower base expon) = let
    operandsAreASAE = isASAE base && isASAE expon
    wellformed = case expon of
        (BAEInteger _) -> case base of
            (BAESymbol _) -> True
            (BAESum _) -> True
            (BAEFunction _ _) -> True
            _ -> False
        _ -> (base /= BAEInteger 0) && (base /= BAEInteger 1)
    in operandsAreASAE && (expon /= BAEInteger 0) && (expon /= BAEInteger 1) && wellformed

asae_8 (BAEFunction sym expr) = isASAE expr