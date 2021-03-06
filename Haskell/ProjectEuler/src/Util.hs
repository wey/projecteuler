module Util where

import Data.List as List
import Data.Map as Map


triangleNumbers :: [Integer]
triangleNumbers = scanl (+) 1 [2..]

fibonacci :: [Integer]
fibonacci = 0 : scanl (+) 1 fibonacci

primeFactors :: Integer -> [Integer]
primeFactors number = reverse $ primeFactors' number [] where
    primeFactors' n foundFactors = case found n of  
        Nothing -> foundFactors
        Just factor -> primeFactors' (n `div` factor) (factor : foundFactors)
    found n = find (isDivisibleBy n) (primesUpTo n)

primesUpTo :: Integer -> [Integer]
primesUpTo n = primes [2..n]

primes :: [Integer] -> [Integer]
primes numbers = sieve numbers Map.empty where
    sieve [] _ = []
    sieve (x:xs) table = case Map.lookup x table of
            Nothing -> x : sieve xs (Map.insert (x*x) [x] table)
            Just facts -> sieve xs $ List.foldl reinsert (Map.delete x table) facts where
                reinsert t prime = Map.insertWith (++) (x+prime) [prime] t

isPrime :: Integer -> Bool
isPrime n = divisorCount n == 2
                
isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy number by = number `mod` by == 0 

divisorCount :: Integer -> Integer
divisorCount n = product summands where
    summands = List.map addOne primeFactorExponents where
        addOne x = toInteger $ x + 1
    primeFactorExponents = List.map length $ group (primeFactors n)
        
divisors :: Integer -> [Integer]
divisors n = 1 : n : List.filter (isDivisibleBy n) [2..(n `div` 2)]
        
noverk :: Integer -> Integer -> Integer
noverk _ 0 = 1
noverk 0 _ = 0
noverk n k = noverk (n-1) (k-1) * n `div` k 















