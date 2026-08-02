module Math.Algebra.ChainComplex.Properties where

import Control.Category.Constrained (id, (.))
import Control.Monad (forM_, unless)
import Math.Algebra.ChainComplex
import Test.Hspec
import Prelude hiding (id, (.))

isEqOn :: (ChainComplex a, ChainComplex b, Show (Basis a), Show (Basis b)) => Basis a -> Morphism a b -> Morphism a b -> Expectation
isEqOn b m m' =
  unless ((m `onBasis` b) == (m' `onBasis` b)) $
    expectationFailure $
      "Images of " ++ show b ++ " are the non-equal " ++ show (m `onBasis` b) ++ " and " ++ show (m' `onBasis` b)

isIdOn :: (ChainComplex a, Show (Basis a)) => Basis a -> Morphism a a -> Expectation
isIdOn b m = isEqOn b m id

isZeroOn :: (ChainComplex a, ChainComplex b, Show (Basis a), Show (Basis b)) => Basis a -> Morphism a b -> Expectation
isZeroOn b m = isEqOn b m 0

isEqOnAll :: (ChainComplex a, ChainComplex b, Show (Basis a), Show (Basis b)) => (Morphism a b, Morphism a b) -> [Basis a] -> Expectation
isEqOnAll (m, m') bs = forM_ bs (\b -> isEqOn b m m')

isIdOnAll :: (ChainComplex a, Show (Basis a)) => Morphism a a -> [Basis a] -> Expectation
isIdOnAll m bs = forM_ bs (\b -> isIdOn b m)

isZeroOnAll :: (ChainComplex a, ChainComplex b, Show (Basis a), Show (Basis b)) => Morphism a b -> [Basis a] -> Expectation
isZeroOnAll m bs = forM_ bs (\b -> isZeroOn b m)

checkChainConditionOn :: (ChainComplex a, Show (Basis a)) => a -> [Basis a] -> Spec
checkChainConditionOn a as = do
  it "the differential should have degree -1" $
    morphismDegree (diff a) `shouldBe` (-1)
  it "images should be valid" $
    forM_ as (\b -> diff a `onBasis` b `shouldSatisfy` validComb a)
  it "∂ ∘ ∂ = 0" $ (diff a . diff a) `isZeroOnAll` as

checkChainCondition :: (FiniteType a, Show (Basis a)) => a -> Int -> Spec
checkChainCondition a n = checkChainConditionOn a ([0 .. n] >>= basis a)

checkChainMap :: (ChainComplex a, ChainComplex a', Show (Basis a'), Show (Basis a)) => a -> a' -> String -> [Basis a] -> Morphism a a' -> Spec
checkChainMap a a' name as m = do
  it "images should be valid" $
    forM_ as (\b -> m `onBasis` b `shouldSatisfy` validComb a')
  it ("∂ ∘ " ++ name ++ " = " ++ name ++ " ∘ ∂") $ (diff a' . m, m . diff a) `isEqOnAll` as

checkIsoOn :: (ChainComplex a, ChainComplex b, Show (Basis a), Show (Basis b)) => [Basis a] -> [Basis b] -> Morphism a b -> Morphism b a -> Expectation
checkIsoOn as bs m m' = (m' . m) `isIdOnAll` as >> (m . m') `isIdOnAll` bs

checkIso ::
  (FiniteType a, FiniteType b, Show (Basis a), Show (Basis b)) =>
  Int ->
  a ->
  b ->
  Morphism a b ->
  Morphism b a ->
  Expectation
checkIso n a b m m' = do
  let as = [0 .. n] >>= basis a
  let bs = [0 .. n] >>= basis b

  checkIsoOn as bs m m'
