module Math.Topology.SSetSpec where

import Control.Monad (forM_)
import Data.Maybe (isJust)
import Test.Hspec

import Math.Topology.SSet
import Math.Topology.SSet.NSimplex
import Math.Topology.SSet.Sphere

recursiveIsSimplex' :: SSet a => a -> Simplex a -> Maybe Int
recursiveIsSimplex' space (NonDegen simplex)
  | isGeomSimplex space simplex = Just (geomSimplexDim space simplex)
  | otherwise = Nothing
recursiveIsSimplex' space (Degen i simplex) = do
  dimension <- recursiveIsSimplex' space simplex
  if i <= dimension then Just (dimension + 1) else Nothing

recursiveSimplexDim :: SSet a => a -> Simplex a -> Int
recursiveSimplexDim space (NonDegen simplex) = geomSimplexDim space simplex
recursiveSimplexDim space (Degen _ simplex) = 1 + recursiveSimplexDim space simplex

recursiveFace :: SSet a => a -> Simplex a -> Int -> Simplex a
recursiveFace space (NonDegen simplex) i = geomFace space simplex i
recursiveFace space (Degen j simplex) i
  | i < j = degen (recursiveFace space simplex i) (j - 1)
  | i > j + 1 = degen (recursiveFace space simplex (i - 1)) j
  | otherwise = simplex

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

    it "evaluates masks like the recursive representation" $ do
      let space = NSimplex 4
          geom = NSimplexSimplex [0, 1, 2]
      forM_ ([0 .. 255] :: [Word]) $ \mask -> do
        let simplex = FormalDegen mask geom
        isSimplex space simplex `shouldBe` isJust (recursiveIsSimplex' space simplex)
        simplexDim space simplex `shouldBe` recursiveSimplexDim space simplex
        forM_ [0 .. simplexDim space simplex] $ \i ->
          face space simplex i `shouldBe` recursiveFace space simplex i
      forM_ ([0 .. 255] :: [Word]) $ \mask -> do
        let space = Sphere 2
            simplex = FormalDegen mask Cell
        forM_ [0 .. simplexDim space simplex] $ \i ->
          face space simplex i `shouldBe` recursiveFace space simplex i

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
