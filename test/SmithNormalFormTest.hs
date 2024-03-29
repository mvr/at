module SmithNormalFormTest where

import Test.Hspec
import Test.QuickCheck

import Data.List (dropWhileEnd, sort)
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import qualified Data.Vector as V

import Math.Algebra.SmithNormalForm

import ArbitraryInstances ()

isDiagonal :: (Eq a, Num a) => Matrix a -> Bool
isDiagonal m = m == M.extendTo 0 r c (M.diagonal 0 (M.getDiag m))
  where
    r = M.nrows m
    c = M.ncols m

spec :: Spec
spec = do
  describe "Smith Normal Form" $ do
    it "multiplies to the original matrix" $
      property $
        \m ->
          let (Triple _ l d r _) = smithNormalForm m
           in m == l * d * r

    it "results in a diagonal matrix" $
      property $
        \m ->
          let (Triple _ _ d _ _) = smithNormalForm m
           in isDiagonal d

    it "has entries in order" $
      property $
        \m ->
          let (Triple _ _ d _ _) = smithNormalForm m
              l = dropWhileEnd (== 0) $ V.toList (M.getDiag d)
           in sort l == l

    it "computes L inverse" $
      property $
        \m ->
          let (Triple li l _ _ _) = smithNormalForm m
           in all (== 1) $ V.toList (M.getDiag (li * l))

    it "computes R inverse" $
      property $
        \m ->
          let (Triple _ _ _ r ri) = smithNormalForm m
           in all (== 1) $ V.toList (M.getDiag (r * ri))

    it "works for bad case 1" $
      let m = M.fromList 2 3 [1, 0, 0, 0, 0, 1]
       in middle (smithNormalForm m) `shouldBe` M.fromList 2 3 [1, 0, 0, 0, 1, 0]
