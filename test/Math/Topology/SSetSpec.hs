module Math.Topology.SSetSpec where

import Test.Hspec

import Math.Topology.SSet
import Math.Topology.SSet.NSimplex

spec :: Spec
spec = do
  describe "formal degeneracies" $ do
    it "normalises inserted degeneracies" $ do
      let simplex = degen (Degen 1 (NonDegen 'x')) 0
      simplex `shouldBe` Degen 2 (Degen 0 (NonDegen 'x'))
      degenList simplex `shouldBe` [2, 0]

    it "composes degeneracies through substitution" $ do
      let simplex = Degen 2 (Degen 0 (NonDegen 'x'))
          substituted = simplex >>= const (Degen 1 (NonDegen 'y'))
      substituted `shouldBe` Degen 3 (Degen 2 (Degen 0 (NonDegen 'y')))

    it "constructs constant simplices" $ do
      let simplex = constantAt 'x' 4
      degenList simplex `shouldBe` [3, 2, 1, 0]
      degenCount simplex `shouldBe` 4
      map (isImageOfDegen simplex) [0 .. 4]
        `shouldBe` [True, True, True, True, False]

  describe "face operators" $ do
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
