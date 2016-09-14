module SmithNormalFormTest where

import Test.Hspec
import Test.QuickCheck

import Data.Matrix
import Math.Algebra.SmithNormalForm

import ArbitraryMatrix()

isDiagonal :: (Eq a, Num a) => Matrix a -> Bool
isDiagonal m = m == (extendTo 0 r c $ diagonal 0 (getDiag m))
  where r = nrows m
        c = ncols m

spec = do
  describe "Smith Normal Form" $ do
    it "multiplies to the original matrix" $ property $
      \m -> let (Triple l d r) = smithNormalForm m in
            m == l*d*r

    it "results in a diagonal matrix" $ property $
      \m -> let (Triple _ d _) = smithNormalForm m in
            isDiagonal d
