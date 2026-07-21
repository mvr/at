module Math.Topology.SSetSpec where

import Test.Hspec

import Math.Topology.SSet
import Math.Topology.SSet.NSimplex

spec :: Spec
spec = describe "face operators" $ do
  it "apply a composite face to a simplex" $ do
    let simplex = NonDegen (NSimplexSimplex [0, 1, 2, 3])
        operator =
          faceOperatorFace
            (faceOperatorFace (identityFaceOperator 3) 3)
            0
    applyFaceOperator (NSimplex 3) operator simplex
      `shouldBe` NonDegen (NSimplexSimplex [1, 2])

  it "identify composites related by a simplicial face identity" $ do
    let operator = identityFaceOperator 3
    faceOperatorFace (faceOperatorFace operator 3) 1
      `shouldBe` faceOperatorFace (faceOperatorFace operator 1) 2
