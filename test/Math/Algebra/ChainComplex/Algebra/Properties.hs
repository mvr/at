module Math.Algebra.ChainComplex.Algebra.Properties where

import Control.Category.Constrained (id, (.))
import Control.Monad (forM_)
import Prelude hiding (id, (.))
import Test.Hspec

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.Combination

import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties

check :: (Algebra a, FiniteType a, Show (Basis a)) => Int -> a -> Spec
check n a = do
  let multiplication = muMor a
      unit = unitMor a
      productComplex = Tensor a a
      productBasis = [0 .. n] >>= basis productComplex
      algebraBasis = [0 .. n] >>= basis a

  it "has degree-zero unit and multiplication" $ do
    morphismDegree unit `shouldBe` 0
    morphismDegree multiplication `shouldBe` 0

  it "unit and multiplication preserve degrees" $ do
    forM_ (coeffs (unit `onBasis` ())) $ \(_, output) ->
      degree a output `shouldBe` 0
    forM_ productBasis $ \input ->
      forM_ (coeffs (multiplication `onBasis` input)) $ \(_, output) ->
        degree a output `shouldBe` degree productComplex input

  ChainComplexProperties.checkChainMap () a "unit" [()] unit
  ChainComplexProperties.checkChainMap productComplex a "multiplication" productBasis multiplication

  it "has a left unit" $
    let leftUnit = multiplication . tensorFunc () a unit id . tensorUnitLInv
     in leftUnit `ChainComplexProperties.isIdOnAll` algebraBasis

  it "has a right unit" $
    let rightUnit = multiplication . tensorFunc a () id unit . tensorUnitRInv
     in rightUnit `ChainComplexProperties.isIdOnAll` algebraBasis

  it "is associative" $ do
    let triples = Tensor (Tensor a a) a
        tripleBasis = [0 .. n] >>= basis triples
        multiplyLeft = multiplication . tensorFunc productComplex a multiplication id
        multiplyRight = multiplication . tensorFunc a productComplex id multiplication . tensorAssoc
    (multiplyLeft, multiplyRight) `ChainComplexProperties.isEqOnAll` tripleBasis
