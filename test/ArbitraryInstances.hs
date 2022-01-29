{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ArbitraryInstances where

import Data.List (sort)
import Data.Matrix
import Math.ValueCategory
import Math.Algebra.AbGroupPres
import Math.Algebra.AbGroupPres.IsoClass
import System.Random
import Test.QuickCheck

-- import Math.ValueCategory.Abelian.Cached

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

arbitraryMatrixSmallEntries :: (Random a, Arbitrary a, Num a) => Gen (Matrix a)
arbitraryMatrixSmallEntries = sized $ \i -> do
  let s = max 1 (min i 5)
  rows <- choose (1, s)
  cols <- choose (1, s)
  elts <- vectorOf (rows * cols) $ choose (0, 10)
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
      prime <- oneof $ fmap return [2, 3, 5, 7, 11]
      power <- choose (1, fromIntegral s)
      return (prime, power)
    return $ IsoClass rank (sort torsion)

instance Arbitrary AbGroupPres where
  arbitrary = fromPresentation <$> arbitraryMatrix

instance Arbitrary (Arrow AbGroupPres) where
  arbitrary = sized $ \i -> do
    let s = max 2 (min i 10)
    domr <- choose (1, s)
    codr <- choose (1, s)

    m <- arbitraryMatrixOfSize codr domr
    return $ morphismFromReducedMatrix (freeAbGroup $ fromIntegral domr) (freeAbGroup $ fromIntegral codr) m

-- instance (AbelianCategory a, Eq a, Arbitrary a) => Arbitrary (Cached a) where
--   arbitrary = fmap toCached arbitrary

-- instance (AbelianCategory a, Eq a, Eq (Morphism a), Arbitrary (Morphism a)) => Arbitrary (CachedMorphism a) where
--   arbitrary = fmap toCachedMorphism arbitrary
