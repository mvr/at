module ArbitraryMatrix where

import Data.Matrix
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = do
    rows <- choose (1, 10)
    cols <- choose (1, 10)
    elts <- vectorOf (rows * cols) arbitrary
    return $ fromList rows cols elts
