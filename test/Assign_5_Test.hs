{- Assignment 5 Tests
 - Name: Sarib Kashif
 - Date: November 30 2019
 -}

import Assign_5

import Test.QuickCheck

main :: IO ()
main = do print "Performing Definite Integral Test: "
          quickCheck propDefiniteIntegral1
          quickCheck propDefiniteIntegral2
          quickCheck propDefiniteIntegral3
          print "Performing FunH Test"
          quickCheck propFunH
          print "Performing FunK Test"
          quickCheck propFunK

-- if function is a constant, the integral will be constant * (b - a) 
-- a is 0 so it will be constant * b, and b is randomized
-- if n is negative, change b to 5 and the function will have constant n
--      this is only because b >= a, so b >= 0
propDefiniteIntegral1 :: Double -> Bool
propDefiniteIntegral1 n 
    | n >= 0 = abs(definiteIntegral 0 n (\x -> 5) 1000 - 5 * n) <= 0.00001
    | otherwise = abs(definiteIntegral 0 5 (\x -> n) 1000 - 5 * n) <= 0.00001

-- integral from [a,b] where a = b will output 0
propDefiniteIntegral2 :: Double -> Bool
propDefiniteIntegral2 n = definiteIntegral n n (\x -> x) 1000 == 0
{-
-- as n increases, output becomes more accurate and output stops changing by too much
-- difference between integral with n * 100 and n * 1000 should be very small because 
   it is approaching the correct answer
-}
propDefiniteIntegral3 :: Integer -> Bool
propDefiniteIntegral3 n 
    | n > 1 = definiteIntegral 0 10 (\x -> x^2 + 5) (n * 100) - definiteIntegral 0 10 (\x -> x^2 + 5) (n * 1000) <= 1
    | otherwise = True

-- as n -> infinity, f(n) = 1, so funH n should be farther away from 1 than funH (n * 10)
propFunH :: Integer -> Bool
propFunH n 
    | n > 0 = abs (funH n - 1) > abs (funH (fromIntegral n * 10) - 1)
    | otherwise = True

-- as n -> infinity, f(n) = infinity, so funK n should be smaller than funK (n * 10)
propFunK :: Double -> Bool
propFunK n 
    | n > 1 = funK n < funK (n * 10)
    | otherwise = True