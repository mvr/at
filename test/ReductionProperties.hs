-- |
module ReductionProperties where

import Control.Category.Constrained (id, (.))
import Control.Monad (unless)
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Reduction
import Test.Hspec
import Prelude hiding (id, (.))

isEqInRange :: (FiniteType a, Eq (Basis a'), Show (Basis a'), Show (Basis a)) => Int -> a -> a' -> Morphism a a' -> Morphism a a' -> Expectation
isEqInRange n a a' m m' = sequence_ $ do
  i <- [0 .. n]
  b <- basis a i
  return $
    unless ((m `onBasis` b) == (m' `onBasis` b)) $
      expectationFailure $ "Images of " ++ show b ++ " are the non-equal " ++ show (m `onBasis` b) ++ " and " ++ show (m' `onBasis` b)

isIdInRange :: (FiniteType a, Show (Basis a)) => Int -> a -> Morphism a a -> Expectation
isIdInRange n a m = isEqInRange n a a m id

isZeroInRange :: (FiniteType a, Eq (Basis a'), Show (Basis a'), Show (Basis a)) => Int -> a -> a' -> Morphism a a' -> Expectation
isZeroInRange n a a' m = isEqInRange n a a' m 0

checkBottom :: (ChainComplex a, ChainComplex b, FiniteType b, Show (Basis b), Show (Basis a)) => Int -> a -> b -> Reduction a b -> Spec
checkBottom n a b (Reduction f g h) = do
  it "f ∘ g = id" $ isIdInRange n b (f . g)
  it "h ∘ g = 0" $ isZeroInRange n b a (h . g)

checkTop :: (ChainComplex a, ChainComplex b, FiniteType a, Show (Basis a), Show (Basis b)) => Int -> a -> b -> Reduction a b -> Spec
checkTop n a b (Reduction f g h) = do
  it "h ∘ h = 0" $ isZeroInRange n a a (h . h)
  it "f ∘ h = 0" $ isZeroInRange n a b (f . h)
  it "h ∘ ∂ + ∂ ∘ h = id - g ∘ f" $ isEqInRange n a a (h . diff a + diff a . h) (id - (g . f))

checkChainCondition :: (ChainComplex a, FiniteType a, Show (Basis a)) => Int -> String -> a -> Spec
checkChainCondition n name a = do
  it ("∂ ∘ ∂ = 0 for " ++ name) $ isZeroInRange n a a (diff a . diff a)

checkChainMap :: (FiniteType a, ChainComplex a', Show (Basis a'), Show (Basis a)) => Int -> String -> a -> a' -> Morphism a a' -> Spec
checkChainMap n name a a' m = do
  it ("∂ ∘ " ++ name ++ " = " ++ name ++ " ∘ ∂") $ isEqInRange n a a' (diff a' . m) (m . diff a)

checkIso :: (FiniteType a, FiniteType b, Show (Basis a), Show (Basis b)) => Int -> a -> b -> Morphism a b -> Morphism b a -> Expectation
checkIso n a b m m' = isEqInRange n a a (m' . m) id >> isEqInRange n b b (m . m') id

check :: (FiniteType a, FiniteType b, Show (Basis b), Show (Basis a)) => Int -> a -> b -> Reduction a b -> Spec
check n a b r@(Reduction f g h) = do
  checkChainCondition n "top" a
  checkChainCondition n "bottom" b
  checkChainMap n "f" a b f
  checkChainMap n "g" b a g

  checkBottom n a b r
  checkTop n a b r
