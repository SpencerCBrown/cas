module Main where

import ParseExpr
import Simplify
import Solve

import Data.Either

testinput = "123"

main :: IO ()

main = do
    putStrLn "Enter an equation. Multiplication is denoted by *. Division can be done with the latex symbol \frac{a}{b} where a and b are expressions. Normal mathematical precedence is assumed. Only accepts equations. Does not solve or simplify trigonometric functions, logarithms, or powers with a variable in the exponent."
    input <- getLine
    let result = parseEquation input
    let (steps, solution) = either (\err -> (show err, Equation BAEUndefined BAEUndefined)) (\eq -> solutionSteps ((simplifyEquation . simplifyEquation) eq)) result
    putStrLn steps
    