module Main where

import ParseExpr
import Simplify
import Solve

import Data.Either

testinput = "123"

main :: IO ()
-- main = do
--     input <- getLine
--     let result = parseExpr input
--     let output = either (\err -> show err) (\expr -> printExpr expr) result
--     putStrLn output
main = do
    input <- getLine
    let result = parseEquation input
    let (steps, solution) = either (\err -> (show err, Equation BAEUndefined BAEUndefined)) (\eq -> solutionSteps eq) result
    putStrLn steps
    