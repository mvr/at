module Math.Topology.SSet.SurjectionSpec where

import Test.Hspec

import Math.Topology.SSet.Surjection

spec :: Spec
spec = describe "ordinal surjections" $ do
  it "enumerates surjections in domain-codomain order" $ do
    surjections 1 2 `shouldBe` []
    surjections (-1) 0 `shouldBe` []
    surjections 2 2
      `shouldBe` [Surjection 2 [0, 1]]
    surjections 3 2
      `shouldBe` fmap (Surjection 3) [[0, 1], [0, 2], [1, 2]]

  it "exposes the transition encoding" $ do
    let a = Surjection 4 [1, 3]
    surjectionDomainDegree a `shouldBe` 4
    surjectionCodomainDegree a `shouldBe` 2
    surjectionFibreSizes a `shouldBe` [2, 2, 1]
    surjectionValues a `shouldBe` [0, 0, 1, 1, 2]
    fmap (surjectionValue a) [0 .. 4] `shouldBe` [0, 0, 1, 1, 2]

  it "recognises valid transition lists" $ do
    isValidSurjection (Surjection 4 [1, 3]) `shouldBe` True
    isValidSurjection (Surjection 4 [1, 1]) `shouldBe` False
    isValidSurjection (Surjection 4 [1, 4]) `shouldBe` False
    isValidSurjection (Surjection (-1) []) `shouldBe` False

  it "recognises repeated edges" $ do
    let a = Surjection 4 [1, 3]
    fmap (surjectionRepeatsAt a) [-1 .. 4]
      `shouldBe` [False, True, False, True, False, False]

  it "removes and inserts repeated edges in bulk" $ do
    let a = Surjection 6 [0, 2, 5]
        reduced = Surjection 4 [0, 1, 3]
    removeRepeats [1, 4] a `shouldBe` reduced
    insertRepeats [1, 4] reduced `shouldBe` a

  it "precomposes with coface maps" $ do
    let a = Surjection 4 [1, 3]
    precomposeFace 1 a `shouldBe` Just (Surjection 3 [0, 2])
    precomposeFace 2 a `shouldBe` Just (Surjection 3 [1, 2])
    precomposeFace 1 (Surjection 2 [0, 1]) `shouldBe` Nothing

  it "precomposes with codegeneracy maps" $ do
    let a = Surjection 4 [1, 3]
    precomposeDegeneracy 1 a `shouldBe` Surjection 5 [2, 4]
    precomposeDegeneracy 2 a `shouldBe` Surjection 5 [1, 4]

  it "recovers a surjection after deleting either repeated vertex" $ do
    let a = Surjection 4 [1, 3]
        sa = precomposeDegeneracy 2 a
    precomposeFace 2 sa `shouldBe` Just a
    precomposeFace 3 sa `shouldBe` Just a
