-- |
module SSetProperties where

import Control.Monad (forM_)
import Test.Hspec
import Prelude hiding (id, (.))

import Math.Topology.SSet

checkIdentities :: (FiniteType a, Show (GeomSimplex a)) => a -> GeomSimplex a -> Expectation
checkIdentities a g = do
  let d = geomSimplexDim a g
      s = NonDegen g

  -- it "satisfies the simplicial identity ∂i ∘ ∂j = ∂(j-1) ∘ ∂i if i < j" $
  sequence_ $ do
    j <- [1 .. d]
    i <- [0 .. (j -1)]
    return $ face a (face a s j) i `shouldBe` face a (face a s i) (j -1)

  -- The rest should follow from the formal degeneracy operations but
  -- may as well do them as a sanity check
  -- it "satisfies the simplicial identity si ∘ sj = sj ∘ s(i-1) if i > j" $
  sequence_ $ do
    i <- [1 .. d]
    j <- [0 .. (i -1)]
    return $ degen (degen s j) i `shouldBe` degen (degen s (i - 1)) j

  -- it "satisfies the simplicial identity ∂i ∘ sj = s(j-1) ∘ ∂i if i < j" $
  sequence_ $ do
    j <- [1 .. d]
    i <- [0 .. (j -1)]
    return $ face a (degen s j) i `shouldBe` degen (face a s i) (j - 1)

  -- it "satisfies the simplicial identity ∂j ∘ sj = id" $
  sequence_ $ do
    j <- [0 .. d]
    return $ face a (degen s j) j `shouldBe` s

  -- it "satisfies the simplicial identity ∂(j+1) ∘ sj = id" $
  sequence_ $ do
    j <- [0 .. d]
    return $ face a (degen s j) (j + 1) `shouldBe` s

  -- it "satisfies the simplicial identity ∂i ∘ sj = sj ∘ ∂(i-1) if i > j+1" $
  sequence_ $ do
    i <- [2 .. d]
    j <- [0 .. (i - 2)]
    return $ face a (degen s j) i `shouldBe` degen (face a s (i -1)) j

checkFaces :: (FiniteType a, Show (GeomSimplex a)) => a -> GeomSimplex a -> Expectation
checkFaces a g =
  forM_ (geomFaces a g) (\s -> s `shouldSatisfy` isSimplex a)

checkDims :: (FiniteType a, Show (GeomSimplex a)) => a -> GeomSimplex a -> Expectation
checkDims a g =
  let d = geomSimplexDim a g
   in forM_ (geomFaces a g) (\s -> simplexDim a s `shouldBe` d - 1)

check :: (FiniteType a, Show (GeomSimplex a)) => Int -> a -> Spec
check n a = do
  it "faces should be valid simplices " $
    forM_ [0 .. n] (\i -> forM_ (geomBasis a i) (checkFaces a))
  it "faces should have correct dimensions" $
    forM_ [0 .. n] (\i -> forM_ (geomBasis a i) (checkDims a))
  it "should satisfy the simplicial identities " $
    forM_ [0 .. n] (\i -> forM_ (geomBasis a i) (checkIdentities a))
