{-# LANGUAGE RecordWildCards #-}
module Math.Algebra.AbGroupPres.IsoClass (
  IsoClass(..),
  invariantFactorsToElementaryDivisors,
  elementaryDivisorsToInvariantFactors
) where

import Data.List (intercalate, group, groupBy, sort, transpose)
import Data.Function (on)

data IsoClass = IsoClass
  {
    rank    :: Integer,
    torsion :: [(Integer, Integer)] -- Should be sorted prime + power pairs
  } deriving (Eq)

instance Show IsoClass where
  show IsoClass{..} =
    if rank > 0 || length torsion > 0 then
      intercalate " + " $ (replicate (fromIntegral rank) "Z") ++
                          fmap (\(prime, power) -> if power == 1 then
                                         "Z/" ++ show prime ++ "Z"
                                       else
                                         "(Z/" ++ show prime ++ "Z)^" ++ show power
                                   ) torsion
    else
      "0"

factorsOf :: Integer -> [Integer]
factorsOf = f (head primes) (tail primes) where
  f n ns m
    | m < 2          = []
    | m < n^2        = [m]
    | m `mod` n == 0 = n : f n ns (m `div` n)
    | otherwise      = f (head ns) (tail ns) m

primes :: [Integer]
primes = 2 : filter (\n -> head (factorsOf n) == n) [3,5..]

splitFactors :: Integer -> [(Integer, Integer)]
splitFactors = fmap (\gp -> (head gp, fromIntegral $ length gp)) . group . factorsOf

invariantFactorsToElementaryDivisors :: [Integer] -> [(Integer, Integer)]
invariantFactorsToElementaryDivisors factors = sort $ factors >>= splitFactors

elementaryDivisorsToInvariantFactors :: [(Integer, Integer)] -> [Integer]
elementaryDivisorsToInvariantFactors = reverse .
                                       fmap (product . fmap (\(prime, power) -> prime^power)) .
                                       transpose .
                                       fmap reverse .
                                       groupBy ((==) `on` fst)
