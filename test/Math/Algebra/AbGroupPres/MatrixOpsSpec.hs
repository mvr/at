module Math.Algebra.AbGroupPres.MatrixOpsSpec where

import Control.Monad (forM_)
import Test.Hspec
import Test.QuickCheck

import qualified Data.Matrix as M

import Math.Algebra.AbGroupPres

import TestSupport.ArbitraryInstances

spec :: Spec
spec = do
  describe "matrixKernel" $ do
    it "is the whole domain for matrices with no rows" $
      forM_ [0 .. 3] $
        \n -> matrixKernel (M.zero 0 n) `shouldBe` M.identity n

    it "is empty for matrices with no columns" $
      forM_ [0 .. 3] $
        \n -> matrixKernel (M.zero n 0) `shouldBe` M.zero 0 0

    it "represents a trivial full-rank kernel with no columns" $ do
      let matrix = M.fromList 1 1 [2]
          kernel = matrixKernel matrix
      (M.nrows kernel, M.ncols kernel) `shouldBe` (1, 0)
      matrix * kernel `shouldBe` M.zero 1 0

  describe "matrixKernelModulo" $ do
    it "agrees with the ordinary kernel when there are no relations" $
      property $
        \m -> matrixKernelModulo m (M.zero (M.nrows m) 0) == matrixKernel m

    it "can project a nonempty solution space onto zero coordinates" $
      matrixKernelModulo (M.zero 0 0) (M.zero 0 2) `shouldBe` M.zero 0 2

    it "represents a trivial full-rank kernel with no columns" $ do
      let matrix = M.fromList 2 1 [1, 0]
          modulus = M.fromList 2 1 [0, 1]
          kernel = matrixKernelModulo matrix modulus
      (M.nrows kernel, M.ncols kernel) `shouldBe` (1, 0)
      matrix * kernel `shouldBe` M.zero 2 0

  describe "divideDiag" $ do
    it "divides correctly" $
      property $ do
        srows <- choose (0, 5)
        scols <- choose (0, 5)
        let size = min srows scols
        diag <- vectorOf size $ choose (1, 10)
        let s = M.extendTo 0 srows scols $ M.diagonalList size 0 diag
        xcols <- choose (0, 5)
        x <- arbitraryMatrixOfSize scols xcols
        let a = s * x
        case divideDiag s a of
          Just d -> return $ s * d == a
          Nothing -> return False

  describe "solveMatrix" $ do
    it "solves a system with no equations" $
      solveMatrix (M.zero 0 3) (M.zero 0 2) `shouldBe` Just (M.zero 3 2)

    it "solves a system with no unknowns and a zero right-hand side" $
      solveMatrix (M.zero 3 0) (M.zero 3 2) `shouldBe` Just (M.zero 0 2)

    it "rejects a nonzero right-hand side when there are no unknowns" $
      solveMatrix (M.zero 2 0) (M.fromList 2 1 [0, 1]) `shouldBe` Nothing

    it "solves a system with no right-hand-side columns" $
      solveMatrix (M.fromList 2 3 [1 .. 6]) (M.zero 2 0) `shouldBe` Just (M.zero 3 0)

    it "solves correctly" $
      property $ do
        mrows <- choose (0, 5)
        mcols <- choose (0, 5)
        xcols <- choose (0, 5)
        m <- arbitraryMatrixOfSize mrows mcols
        x <- arbitraryMatrixOfSize mcols xcols
        let a = m * x
        case solveMatrix m a of
          Just d -> return $ m * d == a
          Nothing -> return False
