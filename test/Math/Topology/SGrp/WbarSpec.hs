module Math.Topology.SGrp.WbarSpec where

import Control.Monad (forM_)
import Test.Hspec

import Math.Algebra.ChainComplex.Algebra.Bar
import Math.Algebra.Group
import Math.Topology.SGrp (SGrp (prodMor), isUnit)
import Math.Topology.SGrp.KGn
import Math.Topology.SGrp.Wbar
import qualified Math.Topology.SGrp.Wbar as Wbar
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet
import Math.Topology.SSet.Effective
import Math.Topology.SSet.NChains
import Math.Topology.SSet.Product (prodNormalise)

import qualified Math.Algebra.ChainComplex.DVF.Properties as DVFProperties
import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties
import qualified Math.Algebra.ChainComplex.Reduction.Properties as ReductionProperties
import qualified Math.Topology.SSet.Properties as SSetProperties

spec :: Spec
spec = do
  describe "K(ℤ/3,2)" $ do
    let g = WbarDiscrete (Zmod 3)
        p = Wbar g
    it "normalisation should be invertible" $
      forM_ [0 .. 5] $ \i ->
        forM_ (allSimplices p i) $ \s ->
          Wbar.normalise g (Wbar.unnormalise g s) `shouldBe` s
    it "does not report a false outer degeneracy" $ do
      let ss = [Degen 1 (NonDegen [1, 1]), Degen 1 (NonDegen [1]), Degen 0 (NonDegen []), NonDegen []]
          compressed = wbarSimplex g ss
      compressed `shouldBe` WbarSimplex 12 (take 2 ss)
      Wbar.normalise g ss `shouldBe` NonDegen compressed
    describe "SSet" $
      SSetProperties.check 4 p
    describe "DVF" $
      DVFProperties.check 4 (NChains p)

  describe "effective K(Z/2,3) model" $
    ChainComplexProperties.checkChainCondition (model (Wbar (Wbar KZmod2_1))) 8

  describe "lifted Wbar reduction for K(Z/2,1)" $
    ReductionProperties.check
      6
      (TensorSusp (NChains (Wbar KZmod2_1)))
      (TensorSusp (Bar (NChains KZmod2_1)))
      ( tensorAlgReduction
          (NChains (Wbar KZmod2_1))
          (Bar (NChains KZmod2_1))
          (wbarReduction (Wbar KZmod2_1))
      )

  describe "Wbar reduction for K(Z/2,1)" $
    ReductionProperties.check
      4
      (NChains (Wbar KZmod2_1))
      (Bar (NChains KZmod2_1))
      (wbarReduction (Wbar KZmod2_1))

  describe "Wbar reduction for K(Z/3,1)" $ do
    let g = WbarDiscrete (Zmod 3)
    ReductionProperties.check
      4
      (NChains (Wbar g))
      (Bar (NChains g))
      (wbarReduction (Wbar g))

  describe "bitmask normalisation" $ do
    let g = WbarDiscrete (Zmod 2)
        p = Wbar g
        inputBars degree = sequence [allSimplices g i | i <- reverse [0 .. degree - 1]]
    it "compresses and expands every bar coordinate invertibly" $
      forM_ [0 .. 6] $ \degree ->
        forM_ (inputBars degree) $ \entries -> do
          let compressed@(WbarSimplex _ nonUnits) = wbarSimplex g entries
          expandWbarSimplex g compressed `shouldBe` entries
          all (not . isUnit g) nonUnits `shouldBe` True
    it "rejects unit masks outside the represented dimension" $
      isGeomSimplex p (WbarSimplex 32 []) `shouldBe` False
    it "agrees with the recursive normalisation algorithm" $
      forM_ [0 .. 6] $ \degree ->
        forM_ (inputBars degree) $ \entries ->
          Wbar.normalise g entries `shouldBe` recursiveNormalise g entries
    it "agrees with the recursive unnormalisation algorithm" $
      forM_ [0 .. 6] $ \degree ->
        forM_ (allSimplices p degree) $ \simplex ->
          Wbar.unnormalise g simplex `shouldBe` recursiveUnnormalise g simplex
    it "computes faces without expanding unit coordinates" $
      forM_ [1 .. 5] $ \degree ->
        forM_ (geomBasis p degree) $ \bar ->
          forM_ [0 .. degree] $ \i ->
            geomFace p bar i
              `shouldBe` Wbar.normalise g (recursiveFaceEntries g (expandWbarSimplex g bar) i)
    it "computes products without expanding unit coordinates" $
      forM_ [0 .. 3] $ \degree ->
        forM_ (allSimplices p degree) $ \left ->
          forM_ (allSimplices p degree) $ \right ->
            prodMor p `onSimplex` prodNormalise (left, right)
              `shouldBe` Wbar.normalise g
                [ prodMor g `onSimplex` prodNormalise pair
                  | pair <- zip (Wbar.unnormalise g left) (Wbar.unnormalise g right)
                ]

recursiveNormalise :: Pointed g => g -> [Simplex g] -> Simplex (Wbar g)
recursiveNormalise _ [] = NonDegen (WbarSimplex 0 [])
recursiveNormalise g (s : ss)
  | isUnit g s = degen (recursiveNormalise g ss) 0
  | otherwise =
      downshift $
        fmap
          (\(s', t) -> wbarSimplex g (s' : recursiveUnnormalise g t))
          (prodNormalise (s, recursiveNormalise g ss))

recursiveUnnormalise :: Pointed g => g -> Simplex (Wbar g) -> [Simplex g]
recursiveUnnormalise g (NonDegen bar) = expandWbarSimplex g bar
recursiveUnnormalise g (Degen i s) = recursiveInsertUnit g i (recursiveUnnormalise g s)

recursiveInsertUnit :: Pointed g => g -> Int -> [Simplex g] -> [Simplex g]
recursiveInsertUnit g 0 ss = constantAt (basepoint g) (length ss) : ss
recursiveInsertUnit g i (s : ss) = degen s (i - 1) : recursiveInsertUnit g (i - 1) ss
recursiveInsertUnit _ _ _ = error "recursiveInsertUnit: impossible"

recursiveFaceEntries :: SGrp g => g -> [Simplex g] -> Int -> [Simplex g]
recursiveFaceEntries _ (_ : ss) 0 = ss
recursiveFaceEntries _ [_] 1 = []
recursiveFaceEntries g (s : s' : rest) 1 =
  (prodMor g `onSimplex` prodNormalise (face g s 0, s')) : rest
recursiveFaceEntries g (s : rest) i =
  face g s (i - 1) : recursiveFaceEntries g rest (i - 1)
recursiveFaceEntries _ [] _ = error "recursiveFaceEntries: invalid face index"
