module Math.Algebra.Bicomplex.Properties where

import Control.Category.Constrained ((.))
import Control.Monad (forM_, unless)
import Math.Algebra.Bicomplex
import Math.Algebra.Combination
import Test.Hspec
import Prelude hiding (id, (.))

isEqOnAll :: (Bicomplex a, Show (Bibasis a)) => (Bimorphism a a, Bimorphism a a) -> [Bibasis a] -> Expectation
isEqOnAll (left, right) basisElements =
  forM_ basisElements $ \basisElement ->
    unless (left `onBibasis` basisElement == right `onBibasis` basisElement) $
      expectationFailure $ "Images of " ++ show basisElement ++ " differ"

isZeroOnAll :: (Bicomplex a, Show (Bibasis a)) => Bimorphism a a -> [Bibasis a] -> Expectation
isZeroOnAll morphism basisElements =
  forM_ basisElements $ \basisElement ->
    morphism `onBibasis` basisElement `shouldBe` zeroCombination

checkChainConditions :: (Bicomplex a, Show (Bibasis a)) => a -> [Bibasis a] -> Spec
checkChainConditions a as = do
  it "the vertical differential should have bidegree (0,-1)" $
    bimorphismDegree (vdiff a) `shouldBe` Bidegree (0, -1)
  it "the horizontal differential should have bidegree (-1,0)" $
    bimorphismDegree (hdiff a) `shouldBe` Bidegree (-1, 0)
  it "images under ∂v should be valid" $
    forM_ as (\b -> vdiff a `onBibasis` b `shouldSatisfy` validBicomb a)
  it "images under ∂v should have the right dimension" $
    forM_
      as
      ( \b ->
          let (h, v) = bidegree a b
           in forM_ (coeffs $ vdiff a `onBibasis` b) (\(_, c) -> bidegree a c `shouldBe` (h, v - 1))
      )
  it "∂v ∘ ∂v = 0" $ (vdiff a . vdiff a) `isZeroOnAll` as
  it "images under ∂h should be valid" $
    forM_ as (\b -> hdiff a `onBibasis` b `shouldSatisfy` validBicomb a)
  it "images under ∂h should have the right dimension" $
    forM_
      as
      ( \b ->
          let (h, v) = bidegree a b
           in forM_ (coeffs $ hdiff a `onBibasis` b) (\(_, c) -> bidegree a c `shouldBe` (h - 1, v))
      )

  it "∂h ∘ ∂h = 0" $ (hdiff a . hdiff a) `isZeroOnAll` as

  it "∂v ∘ ∂h = -∂h ∘ ∂v" $ (vdiff a . hdiff a, negate (hdiff a . vdiff a)) `isEqOnAll` as

-- checkChainMap :: (Bicomplex a, Bicomplex a', Show (Bibasis a'), Show (Bibasis a)) => a -> a' -> String -> [Bibasis a] -> Bimorphism a a' -> Spec
-- checkChainMap a a' name as m = do
--   it "images should be valid" $
--     forM_ as (\b -> m `onBibasis` b `shouldSatisfy` validComb a')
--   it ("∂ ∘ " ++ name ++ " = " ++ name ++ " ∘ ∂") $ (diff a' . m, m . diff a) `isEqOnAll` as
