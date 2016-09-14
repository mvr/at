module SmithNormalFormTest where

import Test.Hspec
import Test.QuickCheck
import Debug.Trace

import Data.List (sort, dropWhileEnd)
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import qualified Data.Vector as V

import Math.Algebra.SmithNormalForm

import ArbitraryMatrix()

isDiagonal :: (Eq a, Num a) => Matrix a -> Bool
isDiagonal m = m == (M.extendTo 0 r c $ M.diagonal 0 (M.getDiag m))
  where r = M.nrows m
        c = M.ncols m


spec = do
  describe "Smith Normal Form" $ do
    it "multiplies to the original matrix" $ property $
      \m -> let (Triple l d r) = smithNormalForm m in
            m == l*d*r

    it "results in a diagonal matrix" $ property $
      \m -> let (Triple _ d _) = smithNormalForm m in
            isDiagonal d

    it "has entries in order" $ property $
      \m -> let (Triple _ d _) = smithNormalForm m
                l = dropWhileEnd (==0) $ V.toList (M.getDiag d) in
            sort l == l
