-- |
module BicomplexProperties where

import Control.Category.Constrained (id, (.))
import Control.Monad (forM_, unless)
import Math.Algebra.ChainComplex (coeffs)
import Math.Algebra.Bicomplex
import Test.Hspec
import Prelude hiding (id, (.))

isEqOn :: (Bicomplex a, Eq (Bibasis a'), Show (Bibasis a'), Show (Bibasis a)) => Bibasis a -> Bimorphism a a' -> Bimorphism a a' -> Expectation
isEqOn b m m' =
  unless ((m `onBibasis` b) == (m' `onBibasis` b)) $
    expectationFailure $ "Images of " ++ show b ++ " are the non-equal " ++ show (m `onBibasis` b) ++ " and " ++ show (m' `onBibasis` b)

isIdOn :: (Bicomplex a, Show (Bibasis a)) => Bibasis a -> Bimorphism a a -> Expectation
isIdOn b m = isEqOn b m id

isZeroOn :: (Bicomplex a, Eq (Bibasis a'), Show (Bibasis a'), Show (Bibasis a)) => Bibasis a -> Bimorphism a a' -> Expectation
isZeroOn b m = isEqOn b m 0

isEqOnAll :: (Bicomplex a, Eq (Bibasis a'), Show (Bibasis a'), Show (Bibasis a)) => (Bimorphism a a', Bimorphism a a') -> [Bibasis a] -> Expectation
isEqOnAll (m, m') bs = forM_ bs (\b -> isEqOn b m m')

isIdOnAll :: (Bicomplex a, Show (Bibasis a)) => Bimorphism a a -> [Bibasis a] -> Expectation
isIdOnAll m bs = forM_ bs (\b -> isIdOn b m)

isZeroOnAll :: (Bicomplex a, Eq (Bibasis a'), Show (Bibasis a'), Show (Bibasis a)) => Bimorphism a a' -> [Bibasis a] -> Expectation
isZeroOnAll m bs = forM_ bs (\b -> isZeroOn b m)

checkChainConditions :: (Bicomplex a, Show (Bibasis a)) => a -> [Bibasis a] -> Spec
checkChainConditions a as = do
  it "images under ∂v should be valid" $
    forM_ as (\b -> vdiff a `onBibasis` b `shouldSatisfy` validBicomb a)
  it "images under ∂v should have the right dimension" $
    forM_ as (\b -> let (h,v) = bidegree a b
               in forM_ (coeffs $ vdiff a `onBibasis` b) (\(_, c) -> bidegree a c `shouldBe` (h, v - 1)))
  it "∂v ∘ ∂v = 0" $ (vdiff a . vdiff a) `isZeroOnAll` as
  it "images under ∂h should be valid" $
    forM_ as (\b -> hdiff a `onBibasis` b `shouldSatisfy` validBicomb a)
  it "images under ∂h should have the right dimension" $
    forM_ as (\b -> let (h, v) = bidegree a b
               in forM_ (coeffs $ hdiff a `onBibasis` b) (\(_, c) -> bidegree a c `shouldBe` (h - 1, v)))

  it "∂h ∘ ∂h = 0" $ (hdiff a . hdiff a) `isZeroOnAll` as

  it "∂v ∘ ∂h = -∂h ∘ ∂v" $ (vdiff a . hdiff a, negate (hdiff a . vdiff a)) `isEqOnAll` as

-- checkChainMap :: (Bicomplex a, Bicomplex a', Show (Bibasis a'), Show (Bibasis a)) => a -> a' -> String -> [Bibasis a] -> Bimorphism a a' -> Spec
-- checkChainMap a a' name as m = do
--   it "images should be valid" $
--     forM_ as (\b -> m `onBibasis` b `shouldSatisfy` validComb a')
--   it ("∂ ∘ " ++ name ++ " = " ++ name ++ " ∘ ∂") $ (diff a' . m, m . diff a) `isEqOnAll` as
