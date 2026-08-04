module Math.Topology.SGrp.KGn.DoldKanSpec where

import Control.Monad (forM_)
import Test.Hspec

import Math.Algebra.Group
import Math.Topology.SGrp.KGn.DoldKan
import Math.Topology.SGrp.KGn.DoldKan.Cocycle
import Math.Topology.SSet

import qualified Math.Topology.SGrp.Properties as SGrpProperties
import qualified Math.Topology.SSet.Properties as SSetProperties

z2 :: Zmod
z2 = Zmod 2

kz2 :: DoldKanKGn Zmod
kz2 = DoldKanKGn 2 z2

one :: ZmodElement
one = zmodElement z2 (1 :: Integer)

simplex ::
  DoldKanKGn Zmod ->
  Int ->
  [([Int], ZmodElement)] ->
  DoldKanSimplex ZmodElement
simplex target q summands =
  doldKanSimplex target q (toSummand <$> summands)
  where
    toSummand (transitions, value) =
      (DoldKanSurjection q transitions, value)

spec :: Spec
spec = do
  describe "single-degree inverse Dold-Kan" $ do
    it "indexes summands by monotone surjections" $ do
      doldKanSurjections 2 1 `shouldBe` []
      doldKanSurjections 0 (-1) `shouldBe` []
      doldKanSurjections 2 2
        `shouldBe` [DoldKanSurjection 2 [0, 1]]
      doldKanSurjections 2 3
        `shouldBe` fmap (DoldKanSurjection 3) [[0, 1], [0, 2], [1, 2]]
      doldKanSurjectionValues (DoldKanSurjection 4 [1, 3])
        `shouldBe` [0, 0, 1, 1, 2]

    it "canonicalises sparse summands in the smart constructor" $
      doldKanSimplex
        kz2
        3
        [ (DoldKanSurjection 3 [1, 2], one),
          (DoldKanSurjection 3 [0, 2], one),
          (DoldKanSurjection 3 [1, 2], one)
        ]
        `shouldBe` simplex kz2 3 [([0, 2], one)]

    it "rejects noncanonical raw records as geometric simplices" $ do
      let unsorted =
            DoldKanSimplex
              3
              [ (DoldKanSurjection 3 [1, 2], one),
                (DoldKanSurjection 3 [0, 1], one)
              ]
          incompatible =
            DoldKanSimplex
              3
              [(DoldKanSurjection 2 [0, 1], one)]
          withUnit =
            DoldKanSimplex
              3
              [ (DoldKanSurjection 3 [0, 1], unit z2),
                (DoldKanSurjection 3 [0, 2], one),
                (DoldKanSurjection 3 [1, 2], one)
              ]
      isGeomSimplex kz2 (DoldKanGeomSimplex unsorted) `shouldBe` False
      isGeomSimplex kz2 (DoldKanGeomSimplex incompatible) `shouldBe` False
      isGeomSimplex kz2 (DoldKanGeomSimplex withUnit) `shouldBe` False

    it "factors common repeat positions as formal degeneracies" $ do
      normalise (simplex kz2 3 [([0, 2], one)])
        `shouldBe` degen
          (NonDegen (DoldKanGeomSimplex (simplex kz2 2 [([0, 1], one)])))
          1
      normalise (simplex kz2 3 [([0, 1], one)])
        `shouldBe` degen
          (NonDegen (DoldKanGeomSimplex (simplex kz2 2 [([0, 1], one)])))
          2

    it "retains vectors with no common repeat position as nondegenerate" $
      normalise
        (simplex kz2 3 [([0, 1], one), ([1, 2], one)])
        `shouldBe` NonDegen
          (DoldKanGeomSimplex (simplex kz2 3 [([0, 1], one), ([1, 2], one)]))

    it "treats distinct surjection summands independently" $
      normalise
        (simplex kz2 3 [([0, 2], one), ([1, 2], one)])
        `shouldBe` NonDegen
          (DoldKanGeomSimplex (simplex kz2 3 [([0, 2], one), ([1, 2], one)]))

    it "represents the zero vector by the constant simplex" $
      normalise (simplex kz2 3 [])
        `shouldBe` constantAt (geomBasepoint kz2) 3

    it "expands formal degeneracies back into coordinates" $ do
      let values = simplex kz2 4 [([0, 2], one)]
      unnormalise (normalise values) `shouldBe` values

    it "gives the fundamental n-simplex constant faces" $
      map
        (face kz2 (NonDegen (DoldKanGeomSimplex (simplex kz2 2 [([0, 1], one)]))))
        [0 .. 2]
        `shouldBe` replicate 3 (constantAt (geomBasepoint kz2) 1)

    it "composes face maps with the indexing surjections" $ do
      let kz1 = DoldKanKGn 1 z2
          edge = NonDegen (DoldKanGeomSimplex (simplex kz1 1 [([0], one)]))
          twoSimplex =
            NonDegen
              (DoldKanGeomSimplex (simplex kz1 2 [([0], one), ([1], one)]))
      map (face kz1 twoSimplex) [0 .. 2]
        `shouldBe` [edge, constantAt (geomBasepoint kz1) 1, edge]

    it "has one free face value per coordinate" $
      fmap (length . allSimplices kz2) [0 .. 4]
        `shouldBe` [1, 1, 2, 8, 64]

  describe "DoldKanKGn SSet" $
    SSetProperties.check 4 kz2

  describe "DoldKanKGn SAb" $
    SGrpProperties.check 3 kz2

  describe "degree-zero DoldKanKGn" $ do
    let degreeZero = DoldKanKGn 0 z2

    it "is the constant simplicial coefficient group" $
      fmap (length . allSimplices degreeZero) [0 .. 3]
        `shouldBe` replicate 4 2

    SSetProperties.check 3 degreeZero

  describe "normalized cocycle coordinates" $ do
    it "recovers coordinate values from Dold-Kan summands" $ do
      let values = CocycleFaceValues 3 [([0, 1], one), ([1, 2], one)]
      doldKanToCocycleValues kz2 (cocycleValuesToDoldKan kz2 values)
        `shouldBe` values

    it "recovers every Dold-Kan simplex from its coordinate values" $
      forM_ ([0 .. 4] >>= allSimplices kz2) $ \s -> do
        let simplexValues = unnormalise s
        cocycleValuesToDoldKan
          kz2
          (doldKanToCocycleValues kz2 simplexValues)
          `shouldBe` simplexValues
