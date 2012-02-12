-- Please see
--   http://laufer.cs.luc.edu/teaching/372/handouts/recursion
-- for detailed notes.

module Primes where

import Data.List (unfoldr)

withoutMultiplesOf   :: Int -> [Int] -> [Int]
withoutMultiplesOf x = filter ((/= 0) . (flip mod $ x))

primesR []     = []
primesR (x:xs) = x : (primesR (withoutMultiplesOf x xs))

primesFold :: [Int] -> [Int]
primesFold = foldr (\y ys -> y : withoutMultiplesOf y ys) []

primesUnfold :: [Int] -> [Int]
primesUnfold = unfoldr g where
  g []     = Nothing
  g (x:xs) = Just (x, withoutMultiplesOf x xs)
