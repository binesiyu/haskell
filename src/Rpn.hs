module Rpn(
          solveRPN
          ,solveRPN'
          ,testRPN1
          ,testRPNFloat
          ) where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
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

solveRPNSafe :: String -> Maybe Double
solveRPNSafe st =  do
        [result] <- foldM foldingFunctionSafe [] (words st)
        return result

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _        -> Nothing

foldingFunctionSafe :: [Double] -> String -> Maybe [Double]
foldingFunctionSafe (x:y:ys) "*"    = return ((x*y):ys)
foldingFunctionSafe (x:y:ys) "+"    = return ((x+y):ys)
foldingFunctionSafe (x:y:ys) "-"    = return ((y-x):ys)
foldingFunctionSafe xs numberString = liftM (:xs) (readMaybe numberString)

solveRPNLog ::  String -> Writer [String] [Double]
solveRPNLog st = foldM foldingFunction [] ( words st)
    where foldingFunction :: [Double] -> String -> Writer [String] [Double]
          foldingFunction (x:y:ys) "*"    = do
                                               tell [show x ++ " * "  ++ show y]
                                               return ((x*y):ys)
          foldingFunction (x:y:ys) "+"    = do
                                               tell [show x ++ " + "  ++ show y]
                                               return ((x+y):ys)
          foldingFunction (x:y:ys) "-"    = do
                                               tell [show x ++ " - "  ++ show y]
                                               return ((y-x):ys)
          foldingFunction xs numberString = do
                                               tell ["read Num " ++ numberString]
                                               return (read numberString:xs)

solveRPNLogWithSafe ::  String -> WriterT [String] Maybe [Double]
solveRPNLogWithSafe st = foldM foldingFunction [] ( words st)
    where foldingFunction :: [Double] -> String -> WriterT [String] Maybe [Double]
          foldingFunction (x:y:ys) "*"    = do
                                               tell [show x ++ " * "  ++ show y]
                                               return ((x*y):ys)
          foldingFunction (x:y:ys) "+"    = do
                                               tell [show x ++ " + "  ++ show y]
                                               return ((x+y):ys)
          foldingFunction (x:y:ys) "-"    = do
                                               tell [show x ++ " - "  ++ show y]
                                               return ((y-x):ys)
          foldingFunction xs numberString = do
                                               tell ["read Num " ++ numberString]
                                               lift $ (:xs) <$> (readMaybe numberString)
