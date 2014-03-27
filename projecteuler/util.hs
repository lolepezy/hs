module Util 
where

intSqrt :: Integral i => i -> Maybe i
intSqrt x | x < 0 = Nothing
intSqrt 0 = Just 0
intSqrt 1 = Just 1
intSqrt n = bSearch 0 n
  where 
   bSearch a b
     | a == b = if a * a == n then Just a else Nothing
     | a == b - 1 = if a * a == n 
        then Just a 
        else bSearch b b    	 
     | m * m < n = bSearch m b 
   	 | m * m >= n = bSearch a m
     where m = quot (a + b) 2

