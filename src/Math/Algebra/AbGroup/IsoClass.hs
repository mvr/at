{-# LANGUAGE RecordWildCards #-}
module Math.Algebra.AbGroup.IsoClass (
  IsoClass(..),
  fromInvariantFactors
) where

import Data.List (intercalate, group, sort)

data IsoClass = IsoClass
  {
    rank    :: Integer,
    torsion :: [Integer] -- Should be sorted prime powers
  } deriving Eq

instance Show IsoClass where
  show IsoClass{..} =
    if rank > 0 || length torsion > 0 then
      intercalate " + " $ (replicate (fromIntegral rank) "Z") ++
                          fmap (\ps -> if length ps == 1 then
                                         "Z/" ++ show (head ps) ++ "Z"
                                       else
                                         "(Z/" ++ show (head ps) ++ "Z)^" ++ show (length ps)
                                   ) (group torsion)
    else
      "0"

factors :: Integer -> [Integer]
factors = f (head primes) (tail primes) where
  f n ns m
    | m < 2          = []
    | m < n^2        = [m]
    | m `mod` n == 0 = n : f n ns (m `div` n)
    | otherwise      = f (head ns) (tail ns) m

primes :: [Integer]
primes = 2 : filter (\n -> head (factors n) == n) [3,5..]

splitFactors :: Integer -> [Integer]
splitFactors = fmap product . group . factors

fromInvariantFactors :: Integer -> [Integer] -> IsoClass
fromInvariantFactors rank factors =
  IsoClass rank (sort $ factors >>= splitFactors)
