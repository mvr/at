module SmithNormalFormTest where

import Test.Hspec
import Test.QuickCheck

import Math.Algebra.SmithNormalForm

import ArbitraryMatrix()

spec = do
  describe "Smith Normal Form" $ do
    it "multiplies to the original matrix" $ property $
      \m -> let (Triple l d r) = smithNormalForm m in
            m == l*d*r

    it "results in a diagonal matrix" $ pending
