-- |
module ChainComplexProperties where

import Control.Category.Constrained (id, (.))
import Control.Monad (forM_, unless)
import Math.Algebra.ChainComplex
import Test.Hspec
import Prelude hiding (id, (.))

isEqOn :: (Num d, Eq a', Show a', Show a) => a -> UMorphism d a a' -> UMorphism d a a' -> Expectation
isEqOn b m m' =
  unless ((m `onBasis` b) == (m' `onBasis` b)) $
    expectationFailure $ "Images of " ++ show b ++ " are the non-equal " ++ show (m `onBasis` b) ++ " and " ++ show (m' `onBasis` b)

isIdOn :: (Num d, Eq a, Show a) => a -> UMorphism d a a -> Expectation
isIdOn b m = isEqOn b m id

isZeroOn :: (Num d, Eq a', Show a', Show a) => a -> UMorphism d a a' -> Expectation
isZeroOn b m = isEqOn b m 0

isEqOnAll :: (Num d, Eq a', Show a', Show a) => (UMorphism d a a', UMorphism d a a') -> [a] -> Expectation
isEqOnAll (m, m') bs = forM_ bs (\b -> isEqOn b m m')

isIdOnAll :: (Num d, Eq a, Show a) => UMorphism d a a -> [a] -> Expectation
isIdOnAll m bs = forM_ bs (\b -> isIdOn b m)

isZeroOnAll :: (Num d, Eq a', Show a', Show a) => UMorphism d a a' -> [a] -> Expectation
isZeroOnAll m bs = forM_ bs (\b -> isZeroOn b m)

checkChainConditionOn :: (ChainComplex a, Show (Basis a)) => a -> String -> [Basis a] -> Spec
checkChainConditionOn a name as = do
  it "images should be valid" $
    forM_ as (\b -> diff a `onBasis` b `shouldSatisfy` validComb a)
  it ("∂ ∘ ∂ = 0 for " ++ name) $ (diff a . diff a) `isZeroOnAll` as

checkChainCondition :: (FiniteType a, Show (Basis a)) => a -> Int -> Spec
checkChainCondition a n = do
  let as = [0 .. n] >>= basis a
  it "images should be valid" $
    forM_ as (\b -> diff a `onBasis` b `shouldSatisfy` validComb a)
  it ("∂ ∘ ∂ = 0") $ (diff a . diff a) `isZeroOnAll` as

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
