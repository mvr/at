module Math.Algebra.AbGroupPres.MatrixOpsSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Matrix as M

import Math.Algebra.AbGroupPres

import TestSupport.ArbitraryInstances

spec :: Spec
spec = do
  describe "matrixKernel" $
    it "represents a trivial full-rank kernel with no columns" $ do
      let matrix = M.fromList 1 1 [2]
          kernel = matrixKernel matrix
      (M.nrows kernel, M.ncols kernel) `shouldBe` (1, 0)
      matrix * kernel `shouldBe` M.zero 1 0

  describe "matrixKernelModulo" $
    it "represents a trivial full-rank kernel with no columns" $ do
      let matrix = M.fromList 2 1 [1, 0]
          modulus = M.fromList 2 1 [0, 1]
          kernel = matrixKernelModulo matrix modulus
      (M.nrows kernel, M.ncols kernel) `shouldBe` (1, 0)
      matrix * kernel `shouldBe` M.zero 2 0

  describe "divideDiag" $ do
    it "divides correctly" $
      property $ do
        srows <- choose (1, 5)
        scols <- choose (1, 5)
        let size = min srows scols
        diag <- vectorOf size $ choose (1, 10)
        let s = M.extendTo 0 srows scols $ M.diagonalList size 0 diag
        xcols <- choose (1, 5)
        x <- arbitraryMatrixOfSize scols xcols
        let a = s * x
        case divideDiag s a of
          Just d -> return $ s * d == a
          Nothing -> return False

  describe "solveMatrix" $ do
    it "solves correctly" $
      property $ do
        mrows <- choose (1, 5)
        mcols <- choose (1, 5)
        xcols <- choose (1, 5)
        m <- arbitraryMatrixOfSize mrows mcols
        x <- arbitraryMatrixOfSize mcols xcols
        let a = m * x
        case solveMatrix m a of
          Just d -> return $ m * d == a
          Nothing -> return False
