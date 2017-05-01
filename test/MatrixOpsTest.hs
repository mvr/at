{-# LANGUAGE ScopedTypeVariables #-}
module MatrixOpsTest where

import Test.Hspec
import Test.QuickCheck
import Debug.Trace

import Data.List (sort, dropWhileEnd)
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import qualified Data.Vector as V

import Math.Algebra.AbGroup

import ArbitraryInstances

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
