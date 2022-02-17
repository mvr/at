-- |
module BicomplexProperties where

import Control.Category.Constrained (id, (.))
import Control.Monad (forM_, unless)
import Math.Algebra.Bicomplex
import Math.Algebra.ChainComplex
import Math.Algebra.Combination
import Test.Hspec
import Prelude hiding (id, (.))

import ChainComplexProperties

checkChainConditions :: (Bicomplex a, Show (Bibasis a)) => a -> [Bibasis a] -> Spec
checkChainConditions a as = do
  it "images under ∂v should be valid" $
    forM_ as (\b -> vdiff a `onBasis` b `shouldSatisfy` validBicomb a)
  it "images under ∂v should have the right dimension" $
    forM_
      as
      ( \b ->
          let (h, v) = bidegree a b
           in forM_ (coeffs $ vdiff a `onBasis` b) (\(_, c) -> bidegree a c `shouldBe` (h, v - 1))
      )
  it "∂v ∘ ∂v = 0" $ (vdiff a . vdiff a) `isZeroOnAll` as
  it "images under ∂h should be valid" $
    forM_ as (\b -> hdiff a `onBasis` b `shouldSatisfy` validBicomb a)
  it "images under ∂h should have the right dimension" $
    forM_
      as
      ( \b ->
          let (h, v) = bidegree a b
           in forM_ (coeffs $ hdiff a `onBasis` b) (\(_, c) -> bidegree a c `shouldBe` (h - 1, v))
      )

  it "∂h ∘ ∂h = 0" $ (hdiff a . hdiff a) `isZeroOnAll` as

  it "∂v ∘ ∂h = -∂h ∘ ∂v" $ (vdiff a . hdiff a, negate (hdiff a . vdiff a)) `isEqOnAll` as

-- checkChainMap :: (Bicomplex a, Bicomplex a', Show (Bibasis a'), Show (Bibasis a)) => a -> a' -> String -> [Bibasis a] -> Bimorphism a a' -> Spec
-- checkChainMap a a' name as m = do
--   it "images should be valid" $
--     forM_ as (\b -> m `onBibasis` b `shouldSatisfy` validComb a')
--   it ("∂ ∘ " ++ name ++ " = " ++ name ++ " ∘ ∂") $ (diff a' . m, m . diff a) `isEqOnAll` as
