{-# LANGUAGE LambdaCase #-}
module Solver
    (
    ) where

import ParseExpr
import Data.List.Ordered

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
    operandsAreASAE = null $ filter (\e -> not isASAE) operands
    noOperandIsProduct = foldl (\accum e -> case e of
        (BAEProduct _) -> False
        _ -> accum) True operands
    atMostOneOperandIsConst = length (filter (\case 
        (BAEInteger _) -> True
        (BAEFraction _) -> True
        _ -> False) operands) <= 1
    in operandsAreASAE && noOperandIsProduct && atMostOneOperandIsConst && isSorted operands

asae_5 (BAESum operands) = let
    operandsAreASAE = null $ filter (\e -> not isASAE) operands
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
        _ -> (base /= 0) && (base /= 1)
    in operandsAreASAE && (expon /= 0) && (expon /= 1) && wellformed

asae_8 (BAEFunction sym expr) = isASAE expr