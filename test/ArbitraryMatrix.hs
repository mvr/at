module ArbitraryMatrix where

import Data.List (sort)

import Data.Matrix
import Test.QuickCheck

import Math.Algebra.AbGroup
import Math.Algebra.AbGroup.IsoClass

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = arbitraryMatrix
  shrink = allMinors

arbitraryMatrixOfSize :: Arbitrary a => Int -> Int -> Gen (Matrix a)
arbitraryMatrixOfSize r c = do
  elts <- vectorOf (r * c) arbitrary
  return $ fromList r c elts

arbitraryMatrix :: Arbitrary a => Gen (Matrix a)
arbitraryMatrix = sized $ \i -> do
  let s = max 1 (min i 5)
  rows <- choose (1, s)
  cols <- choose (1, s)
  arbitraryMatrixOfSize rows cols

arbitraryMatrixOnesZeroes :: (Arbitrary a, Num a) => Gen (Matrix a)
arbitraryMatrixOnesZeroes = sized $ \i -> do
  let s = max 1 (min i 10)
  rows <- choose (1, s)
  cols <- choose (1, s)
  elts <- vectorOf (rows * cols) arbitrary
  return $ fromList rows cols elts

allMinors :: Matrix a -> [Matrix a]
allMinors m = do
  r <- [0 .. (nrows m) - 1]
  c <- [0 .. (ncols m) - 1]
  return $ minorMatrix r c m

instance Arbitrary IsoClass where
  arbitrary = sized $ \i -> do
    let s = max 2 (min i 10)
    rank <- choose (0, fromIntegral s)
    torsionSize <- choose (0, s)
    torsion <- vectorOf torsionSize $ do
      prime <- oneof $ fmap return [2,3,5,7,11]
      power <- choose (1, fromIntegral s)
      return (prime, power)
    return $ IsoClass rank (sort torsion)

instance Arbitrary AbGroup where
  arbitrary = fromPresentation <$> arbitraryMatrix

instance Arbitrary AbMorphism where
  arbitrary = do
    domM <- arbitrary
    codM <- arbitrary

    let dom = fromPresentation domM
        cod = fromPresentation codM
        rows = ncols (reduced dom)
        cols = ncols (reduced cod)

    m <- arbitraryMatrixOfSize rows cols
    return $ morphismFromReducedMatrix dom cod m
