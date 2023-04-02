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

factor :: Int -> [Int]
factor 1 = [1]
factor n = filter (/= 1) $ factor' n primes []
  where primes = prime_sieve sqn
        sqn = ceiling $ sqrt $ fromIntegral n

factor' :: Int -> [Int] -> [Int] -> [Int]
factor' n [] rs = n:rs
factor' n (p:ps) rs = 
  case n `mod` p of
    0 -> factor' (n `div` p) (p:ps) (p:rs)
    _ -> factor' n ps rs

prime_sieve :: Int -> [Int]
prime_sieve n
  | n < 2 = []
  | n == 2 = [2]
  | n <= 4 = [3,2]
  | n <= 6 = [5,3,2]
  | otherwise = (prime_sieve' [3,5..n] []) ++ [2]

prime_sieve' :: [Int] -> [Int] -> [Int]
prime_sieve' [] rs = rs
prime_sieve' (p:ps) [] = prime_sieve' (filter f ps) [p] where f x = (x `mod` p) /= 0
prime_sieve' (p:ps) (r:rs) =
  case (p*r) > (last ps) of
    True -> (reverse (p:ps)) ++ (r:rs)
    False -> prime_sieve' (filter f ps) (p:r:rs) where f x = (x `mod` p) /= 0
