module ArbitraryMatrix where

import Data.Matrix
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = arbitraryMatrix

arbitraryMatrix :: Arbitrary a => Gen (Matrix a)
arbitraryMatrix = sized $ \i -> do
  let s = max 1 (min i 10)
  rows <- choose (1, s)
  cols <- choose (1, s)
  elts <- vectorOf (rows * cols) arbitrary
  return $ fromList rows cols elts

arbitraryMatrixOnesZeroes :: (Arbitrary a, Num a) => Gen (Matrix a)
arbitraryMatrixOnesZeroes = sized $ \i -> do
  let s = max 1 (min i 10)
  rows <- choose (1, s)
  cols <- choose (1, s)
  elts <- vectorOf (rows * cols) arbitrary
  return $ fromList rows cols elts
