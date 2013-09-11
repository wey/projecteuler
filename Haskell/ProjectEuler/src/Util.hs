module Util where

import Data.List as List
import Data.Map as Map

fibonacci :: [Integer]
fibonacci = 0 : scanl (+) 1 fibonacci

primeFactors :: Integer -> [Integer]
primeFactors number = reverse $ primeFactors' number []  
    where
        primeFactors' n foundFactors = case found n of  
            Nothing -> foundFactors
            Just factor -> primeFactors' (n `div` factor) (factor : foundFactors)
        found n = find (isDivisibleBy n) (primesUpTo n)

primesUpTo :: Integer -> [Integer]
primesUpTo n = primes [2..n]

primes :: [Integer] -> [Integer]
primes numbers = sieve numbers Map.empty
    where
        sieve [] _ = []
        sieve (x:xs) table = case Map.lookup x table of
                Nothing -> x : sieve xs (Map.insert (x*x) [x] table)
                Just facts -> sieve xs (List.foldl reinsert (Map.delete x table) facts)
            where
                reinsert t prime = Map.insertWith (++) (x+prime) [prime] t
                
isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy number by = number `mod` by == 0 