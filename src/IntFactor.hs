module IntFactor
( showFactor
, factor
, prime_sieve
) where

import Data.List (groupBy, intercalate)

showFactor :: Int -> String
showFactor n = (show n) ++ " = " ++ (intercalate " * " factored)
  where factored = map (\(x,y) -> (show x) ++ (if y == 1 then "" else "^" ++ (show y))) paired
        paired = map (\xs -> (head xs, length xs)) grouped
        grouped = groupBy (==) (factor n)

-- prime factorization function returning a list of the numbers
factor :: Int -> [Int]
-- unique factorization is only for numbers greater than 1
factor n | n < 2 = error "Numbers less than 2 have non-unique factorizations"
-- test the primes up to the sqrt(n) 
factor n = factor' n primes []
  where primes = prime_sieve sqn
        sqn = ceiling $ sqrt $ fromIntegral n

-- helper function
-- note this is intended to be passed the primes up to the sqrt(n) of the inital tested number
-- first argument is the number being factored (this is not the original value input neccessarily)
-- second argument is the list of primes to test against
-- the third is the cumulative list of factors
factor' :: Int -> [Int] -> [Int] -> [Int]
-- if 1 is the input number we've completely factored the number and just return the prime results
factor' 1 _ rs = rs
-- if there are no values remaining to test return the results with n appended to the end of the list
factor' n [] rs = rs ++ [n]
-- if n % p is 0, call function again with n / p and p as one of the results.
-- if not toss p as a potential factor 
factor' n (p:ps) rs =
  case n `mod` p of
    0 -> factor' (n `div` p) (p:ps) (p:rs)
    _ -> factor' n ps rs

-- prime sieve function 
-- some cases are hardcoded to speed up slightly
prime_sieve :: Int -> [Int]
prime_sieve n
  | n < 2 = [] -- no primes less than 2
  | n == 2 = [2] 
  | n <= 4 = [3,2]
  | n <= 6 = [5,3,2]
  | otherwise = (prime_sieve' [3,5..n] []) ++ [2] -- this fails on 5 and 6 hence hardcoding those values

--helper function
-- first argument is the remaining numbers to be sieved
-- the second is the cumulative list of primes
prime_sieve' :: [Int] -> [Int] -> [Int]
-- if there are no numbers left to test return the results
prime_sieve' [] rs = rs
-- case where we have no primes yet
prime_sieve' (p:ps) [] = prime_sieve' (filter f ps) [p] where f x = (x `mod` p) /= 0
-- if the product of current number to test p times r the last added prime
-- is greater than the last element of numbers to test then remaining numbers must be prime 
-- if not use the classic sieve algorithm
prime_sieve' (p:ps) (r:rs) =
  case (p*r) > (last ps) of
    True -> (reverse (p:ps)) ++ (r:rs)
    False -> prime_sieve' (filter f ps) (p:r:rs) where f x = (x `mod` p) /= 0
