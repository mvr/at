module MatrixOpsTest where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Matrix as M

import Math.Algebra.AbGroupPres

import ArbitraryInstances

spec :: Spec
spec = do
  describe "divideDiag" $ do
    it "divides correctly" $ property $ do
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
    it "solves correctly" $ property $ do
      mrows <- choose (1, 5)
      mcols <- choose (1, 5)
      xcols <- choose (1, 5)
      m <- arbitraryMatrixOfSize mrows mcols
      x <- arbitraryMatrixOfSize mcols xcols
      let a = m * x
      case solveMatrix m a of
        Just d -> return $ m * d == a
        Nothing -> return False
