module Solve
    (
        solutionSteps
    ) where

import ParseExpr
import Simplify

-- checks that a simplified expression only has a single variable
univariate :: BAE -> Bool
univariate expr = undefined

{- checks that a simplified expression does not have a variable raised to a power >= 2
    as we do not solve quadratic equations (or any polynomials of degree >= 2) -}
degreeOne :: BAE -> Bool
degreeOne expr = undefined

data Equation = Equation

solutionSteps :: Equation -> (String, Equation)
solutionSteps = undefined