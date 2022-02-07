-- |
module ReductionProperties where

import Control.Category.Constrained (id, (.))
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Reduction
import Test.Hspec
import Prelude hiding (id, (.))

import ChainComplexProperties

checkBottom :: (ChainComplex a, ChainComplex b, Show (Basis b), Show (Basis a)) => [Basis a] -> [Basis b] -> Reduction a b -> Spec
checkBottom as bs (Reduction f g h) = do
  it "f ∘ g = id" $ (f . g) `isIdOnAll` bs
  it "h ∘ g = 0" $ (h . g) `isZeroOnAll` bs

checkTop :: (ChainComplex a, ChainComplex b, Show (Basis a), Show (Basis b)) => a -> [Basis a] -> [Basis b] -> Reduction a b -> Spec
checkTop a as bs (Reduction f g h) = do
  it "h ∘ h = 0" $ (h . h) `isZeroOnAll` as
  it "f ∘ h = 0" $ (f . h) `isZeroOnAll` as
  it "h ∘ ∂ + ∂ ∘ h = id - g ∘ f" $ (h . diff a + diff a . h, id - (g . f)) `isEqOnAll` as

checkOn :: (ChainComplex a, ChainComplex b, Show (Basis b), Show (Basis a)) => a -> b -> [Basis a] -> [Basis b] -> Reduction a b -> Spec
checkOn a b as bs r@(Reduction f g h) = do
  checkChainCondition a "top" as
  checkChainCondition b "bottom" bs
  checkChainMap a b "f" as f
  checkChainMap b a "g" bs g

  checkBottom as bs r
  checkTop a as bs r

check :: (FiniteType a, FiniteType b, Show (Basis b), Show (Basis a)) => Int -> a -> b -> Reduction a b -> Spec
check n a b r = do
  let as = [0 .. n] >>= basis a
  let bs = [0 .. n] >>= basis b

  checkOn a b as bs r
