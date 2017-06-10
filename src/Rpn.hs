module Rpn(
          solveRPN
          ,solveRPN'
          ,testRPN1
          ,testRPNFloat
          ) where

import           Data.List

solveRPN :: (Num a,Read a) => String -> a
{- solveRPN expression = head (foldl foldingFunction [] (words expression)) -}
    {- where foldingFunction stack item = error "test" -}

solveRPN  = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*"    = (x*y):ys
          foldingFunction (x:y:ys) "+"    = (x+y):ys
          foldingFunction (x:y:ys) "-"    = (y-x):ys
          foldingFunction xs numberString = read numberString:xs

testRPN1 = solveRPN "10 4 3 + 2 * -"

solveRPN' :: String -> Float
solveRPN' = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*"    = (x*y):ys
          foldingFunction (x:y:ys) "+"    = (x+y):ys
          foldingFunction (x:y:ys) "-"    = (y-x):ys
          foldingFunction (x:y:ys) "/"    = (y/x):ys
          foldingFunction (x:y:ys) "^"    = (y ** x):ys
          foldingFunction (x:xs) "ln"     = log x:xs
          foldingFunction xs "sum"        = [sum xs]
          foldingFunction xs numberString = read numberString:xs

testRPNFloat = solveRPN' "2.7 ln"
