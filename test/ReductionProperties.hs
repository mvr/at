-- |
module ReductionProperties where

import Control.Category.Constrained (id, (.))
import Control.Monad (forM_, unless)
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Reduction
import Test.Hspec
import Prelude hiding (id, (.))

isEqOn :: (ChainComplex a, Eq (Basis a'), Show (Basis a'), Show (Basis a)) => Basis a -> Morphism a a' -> Morphism a a' -> Expectation
isEqOn b m m' =
  unless ((m `onBasis` b) == (m' `onBasis` b)) $
    expectationFailure $ "Images of " ++ show b ++ " are the non-equal " ++ show (m `onBasis` b) ++ " and " ++ show (m' `onBasis` b)

isIdOn :: (ChainComplex a, Show (Basis a)) => Basis a -> Morphism a a -> Expectation
isIdOn b m = isEqOn b m id

isZeroOn :: (ChainComplex a, Eq (Basis a'), Show (Basis a'), Show (Basis a)) => Basis a -> Morphism a a' -> Expectation
isZeroOn b m = isEqOn b m 0

isEqOnAll :: (ChainComplex a, Eq (Basis a'), Show (Basis a'), Show (Basis a)) => (Morphism a a', Morphism a a') -> [Basis a] -> Expectation
isEqOnAll (m, m') bs = forM_ bs (\b -> isEqOn b m m')

isIdOnAll :: (ChainComplex a, Show (Basis a)) => Morphism a a -> [Basis a] -> Expectation
isIdOnAll m bs = forM_ bs (\b -> isIdOn b m)

isZeroOnAll :: (ChainComplex a, Eq (Basis a'), Show (Basis a'), Show (Basis a)) => Morphism a a' -> [Basis a] -> Expectation
isZeroOnAll m bs = forM_ bs (\b -> isZeroOn b m)

checkBottom :: (ChainComplex a, ChainComplex b, Show (Basis b), Show (Basis a)) => [Basis a] -> [Basis b] -> Reduction a b -> Spec
checkBottom as bs (Reduction f g h) = do
  it "f ∘ g = id" $ (f . g) `isIdOnAll` bs
  it "h ∘ g = 0" $ (h . g) `isZeroOnAll` bs

checkTop :: (ChainComplex a, ChainComplex b, Show (Basis a), Show (Basis b)) => a -> [Basis a] -> [Basis b] -> Reduction a b -> Spec
checkTop a as bs (Reduction f g h) = do
  it "h ∘ h = 0" $ (h . h) `isZeroOnAll` as
  it "f ∘ h = 0" $ (f . h) `isZeroOnAll` as
  it "h ∘ ∂ + ∂ ∘ h = id - g ∘ f" $ (h . diff a + diff a . h, id - (g . f)) `isEqOnAll` as

checkChainCondition :: (ChainComplex a, Show (Basis a)) => a -> String -> [Basis a] -> Spec
checkChainCondition a name as = do
  it ("∂ ∘ ∂ = 0 for " ++ name) $ (diff a . diff a) `isZeroOnAll` as

checkChainMap :: (ChainComplex a, ChainComplex a', Show (Basis a'), Show (Basis a)) => a -> a' -> String -> [Basis a] -> Morphism a a' -> Spec
checkChainMap a a' name as m = do
  it ("∂ ∘ " ++ name ++ " = " ++ name ++ " ∘ ∂") $ (diff a' . m, m . diff a) `isEqOnAll` as

checkIsoOn :: (ChainComplex a, ChainComplex b, Show (Basis a), Show (Basis b)) => [Basis a] -> [Basis b] -> Morphism a b -> Morphism b a -> Expectation
checkIsoOn as bs m m' = (m' . m) `isIdOnAll` as >> (m . m') `isIdOnAll` bs

checkOn :: (ChainComplex a, ChainComplex b, Show (Basis b), Show (Basis a)) => a -> b -> [Basis a] -> [Basis b] -> Reduction a b -> Spec
checkOn a b as bs r@(Reduction f g h) = do
  checkChainCondition a "top" as
  checkChainCondition b "bottom" bs
  checkChainMap a b "f" as f
  checkChainMap b a "g" bs g

  checkBottom as bs r
  checkTop a as bs r

checkIso ::
  (FiniteType a, FiniteType b, Show (Basis a), Show (Basis b)) =>
  Int ->
  a ->
  b ->
  UMorphism (Basis a) (Basis b) ->
  UMorphism (Basis b) (Basis a) ->
  Expectation
checkIso n a b m m' = do
  let as = [0 .. n] >>= basis a
  let bs = [0 .. n] >>= basis b

  checkIsoOn as bs m m'

check :: (FiniteType a, FiniteType b, Show (Basis b), Show (Basis a)) => Int -> a -> b -> Reduction a b -> Spec
check n a b r = do
  let as = [0 .. n] >>= basis a
  let bs = [0 .. n] >>= basis b

  checkOn a b as bs r
