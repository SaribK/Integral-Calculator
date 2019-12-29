{- Assignment 5
 - Name: Sarib Kashif
 - Date: November 30 2019
 -}
module Assign_5 where

macid :: String
macid = "kashis2"


{- -----------------------------------------------------------------
 - definiteIntegral
 - do the first summation with initial a b g n values
 - call auxillary function with a second a value so that it doesn't change during recursion
 - from calculus: xi = a + delta x * i where delta x = (b - a) / n
 - every time the function is recursed, change the a value (xi) to the 
   next x value by performing the equation above
   e.g if i just calculated x1, then when i call the function again
   a = x2 so that xi is constantly increasing
 - function stops when xi = b because that is the final number
 -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n = deltax * (g a + g (a + deltax))/2 + definiteIntegralAux (a + deltax) a b g n
    where deltax = (b - a) / fromIntegral n

definiteIntegralAux :: Double -> Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegralAux a a' b g n  
    | (b - a <= 0.00000001) = 0
    | otherwise = deltax * (g a + g (a + deltax))/2 + definiteIntegralAux (a + deltax) a' b g n
        where deltax = (b - a') / fromIntegral n

{- -----------------------------------------------------------------
 - funH
 - find the definite integral of x^(1/n) and subtract it by the integral from x^n
 - 10000 is chosen as # of trapezoids since it is a large enough number
 -}
funH :: Integer -> Double
funH n = definiteIntegral 0 1 (\x -> x ** (1/fromIntegral n)) 10000 - definiteIntegral 0 1 (\x -> x ^ n) 10000

{- -----------------------------------------------------------------
 - funK
 - find definite intregal of n ** x
 - don't subtract by integral of x axis because it is always 0 (unnecesary)
 -}
funK :: Double -> Double
funK a = definiteIntegral (-1) 1 (\x -> a ** x) 10000 

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 1
 - - Input: (-100) (-24) (\x -> x^3/(x + 4)) 10 
 - - Expected Output: 362266.3393839817
 - - Acutal Output: 362266.3393839817

 - - Function: definiteIntegral
 - - Test Case Number: 2
 - - Input: (-100) (-24) (\x -> x^3/(x + 4)) 1000 
 - - Expected Output: 349037.7558989381
 - - Acutal Output: 349037.7558989381

 - - Function: definiteIntegral
 - - Test Case Number: 3
 - - Input: (-100) 100 (\x -> log (x^3 + x - 2)) 100
 - - Expected Output: NaN
 - - Acutal Output: NaN

 - - Function: definiteIntegral
 - - Test Case Number: 4
 - - Input: 1.2 100 (\x -> log (x^3 + x - 2)) 100
 - - Expected Output: 1082.8770592330748
 - - Acutal Output: 1082.8770592330748

 - - Function: funH
 - - Test Case Number: 1
 - - Input: 1000
 - - Expected Output: 0.9980016143455841
 - - Acutal Output: 0.9980016143455841

 - - Function: funH
 - - Test Case Number: 2
 - - Input: 100000
 - - Expected Output: 0.9999400005840837
 - - Acutal Output: 0.9999400005840837
 
 - - Function: funH
 - - Test Case Number: 3
 - - Input: 1
 - - Expected Output: 0
 - - Acutal Output: 0

 - - Function: funK
 - - Test Case Number: 1
 - - Input: -1
 - - Expected Output: NaN
 - - Acutal Output: NaN

 - - Function: funK
 - - Test Case Number: 2
 - - Input: 100
 - - Expected Output: 21.703639893670946
 - - Acutal Output: 21.703639893670946

 - - Function: funK
 - - Test Case Number: 3
 - - Input: 1000
 - - Expected Output: 144.67194451880624
 - - Acutal Output: 144.67194451880624

 - - Function: funK
 - - Test Case Number: 4
 - - Input: 10000
 - - Expected Output: 1084.790793134229
 - - Acutal Output: 1084.790793134229
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - QuickCheck

 - Function: definiteIntegral
 - Property: n >= 0 = abs(definiteIntegral 0 n (\x -> 5) 1000 - 5 * n) <= 0.00001
 - Result: Pass

 - Function: definiteIntegral
 - Property: definiteIntegral n n (\x -> x) 1000 == 0
 - Result: Pass

 - Function: definiteIntegral
 - Property: n > 1 = definiteIntegral 0 10 (\x -> x^2 + 5) (n * 100) - definiteIntegral 0 10 (\x -> x^2 + 5) (n * 1000) <= 1
 - Result: Pass

 - Function: funH
 - Property: n > 0 = abs (funH n - 1) > abs (funH (fromIntegral n * 10) - 1)
 - Result: Pass

 - Function: funK
 - Property: n > 1 = funK n < funK (n * 10)
 - Result: Pass

 -}

