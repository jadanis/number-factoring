module IntFactor
( showFactor
, factor
, sieve
) where

import Data.List (group, intercalate)

showFactor :: Int -> String
showFactor n = (show n) ++ " = " ++ (intercalate " * " factored)
  where factored = map (\(x,y) -> (show x) ++ (if (y == 1) then "" else ("^" ++ (show y)))) paired
        paired = map (\xs -> (head xs, length xs)) grouped
        grouped = group $ factor n

factor :: Int -> [Int]
factor n | n < 1 = error "No unique factorization for numbers less than 1"
factor 1 = [1]
factor n = factor' n primes []
  where primes = sieve sqn
        sqn = ceiling $ sqrt $ fromIntegral n

factor' :: Int -> [Int] -> [Int] -> [Int]
factor' 1 _ res = res
factor' n [] res = res ++ [n]
factor' n primes@(p:ps) res =
 case n `mod` p of
  0 -> factor' (n `div` p) primes (p:res)
  _ -> factor' n ps res

sieve :: Int -> [Int]
sieve n
 | n < 2 = []
 | otherwise = sieve' [3,5..n] [2]

sieve' :: [Int] -> [Int] -> [Int]
sieve' [] res = res
sieve' primes@(p:ps) res@(r:rs) =
  case (p*r) > (last primes) of
    True -> (reverse primes) ++ res
    False -> sieve' (filter f ps) (p:res) where f x = (x `mod` p) /= 0
