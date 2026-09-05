module Math.Algebra.ChainComplex.Coalgebra.CobarSpec where

import Control.Monad (forM_, replicateM)
import Test.Hspec

import Math.Algebra.AbGroupPres (freeAbGroup)
import Math.Algebra.Bicomplex hiding (FiniteType)
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Coalgebra
import Math.Algebra.ChainComplex.Coalgebra.Cobar
import Math.Algebra.ChainComplex.Disk
import Math.Algebra.Combination
import Math.Topology.SSet.NChains
import Math.Topology.SSet.Product
import Math.Topology.SSet.Sphere

import qualified Math.Algebra.Bicomplex.Properties as BicomplexProperties
import qualified Math.Algebra.ChainComplex.Algebra.Properties as AlgebraProperties
import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties

data SignCoalgebra = SignCoalgebra

data SignBasis
  = SignUnit
  | EvenPrimitive
  | OddPrimitive
  | DesuspensionBoundary
  | DesuspensionInterior
  | EvenOdd
  | OddEven
  deriving (Eq, Ord, Show)

instance ChainComplex SignCoalgebra where
  type Basis SignCoalgebra = SignBasis

  degree _ SignUnit = 0
  degree _ EvenPrimitive = 2
  degree _ OddPrimitive = 3
  degree _ DesuspensionBoundary = 2
  degree _ DesuspensionInterior = 3
  degree _ EvenOdd = 5
  degree _ OddEven = 5

  diff _ = Morphism (-1) $ \b -> case b of
    DesuspensionInterior -> singleComb DesuspensionBoundary
    _ -> 0

instance BoundedBelow SignCoalgebra where
  lowerBound _ = 0

instance ConnectedChainComplex SignCoalgebra

instance OneReducedChainComplex SignCoalgebra

instance FiniteType SignCoalgebra where
  basis _ 0 = [SignUnit]
  basis _ 2 = [EvenPrimitive, DesuspensionBoundary]
  basis _ 3 = [OddPrimitive, DesuspensionInterior]
  basis _ 5 = [EvenOdd, OddEven]
  basis _ _ = []

instance Coalgebra SignCoalgebra where
  counitMor _ = Morphism 0 $ \b ->
    if b == SignUnit then singleComb () else 0

  delMor _ = Morphism 0 $ \b -> case b of
    SignUnit -> singleComb (SignUnit, SignUnit)
    EvenPrimitive -> primitive EvenPrimitive
    OddPrimitive -> primitive OddPrimitive
    DesuspensionBoundary -> primitive DesuspensionBoundary
    DesuspensionInterior -> primitive DesuspensionInterior
    EvenOdd -> primitive EvenOdd + singleComb (EvenPrimitive, OddPrimitive)
    OddEven -> primitive OddEven + singleComb (OddPrimitive, EvenPrimitive)
    where
      primitive b =
        singleComb (SignUnit, b) + singleComb (b, SignUnit)

instance CoaugmentedCoalgebra SignCoalgebra where
  coaugmentationMor _ = Morphism 0 $ const $ singleComb SignUnit

spec :: Spec
spec = describe "Cobar" $ do
  describe "lower bounds" $ do
    it "bounds the coaugmentation coideal below by two" $
      lowerBound (CoaugmentationCoideal SignCoalgebra) `shouldBe` 2

    it "bounds the tensor algebra below by zero, including the empty word" $ do
      lowerBound (cobarTensor SignCoalgebra) `shouldBe` 0
      basis (cobarTensor SignCoalgebra) 0 `shouldBe` [[]]

    it "preserves the bound when adding the coproduct differential" $
      lowerBound (Cobar SignCoalgebra) `shouldBe` 0

  describe "coideal words" $ do
    let coideal = CoaugmentationCoideal SignCoalgebra

    it "handles empty words and impossible degrees or lengths" $ do
      coidealWords coideal 0 0 `shouldBe` [[]]
      coidealWords coideal 1 0 `shouldBe` []
      coidealWords coideal 0 (-1) `shouldBe` []
      coidealWords coideal 3 2 `shouldBe` []

    it "enumerates each word of the requested length and degree once" $
      forM_ [0 .. 3] $ \l ->
        forM_ [-1 .. 16] $ \d -> do
          let ws = replicateM l ([2 .. 5] >>= basis coideal)
          coidealWords coideal d l
            `shouldMatchList` filter ((== d) . sum . fmap (degree coideal)) ws

  describe "unrestricted tensor desuspension" $ do
    let tensorDesusp = TensorDesusp (Disk 1)

    ChainComplexProperties.checkChainConditionOn
      tensorDesusp
      [[], [DiskBase], [DiskBoundary], [DiskInterior], [DiskInterior, DiskInterior]]

    it "allows degree-one generators" $
      degree tensorDesusp [DiskInterior] `shouldBe` 0

    it "retains their internal differential" $
      diff tensorDesusp `onBasis` [DiskInterior]
        `shouldBe` -(singleComb [DiskBoundary])

  describe "desuspension signs" $ do
    let perturbation = cobarPerturbation SignCoalgebra
        word = id

    it "negates the internal differential on a desuspended generator" $
      diff (cobarTensor SignCoalgebra) `onBasis` word [DesuspensionInterior]
        `shouldBe` -(singleComb (word [DesuspensionBoundary]))

    it "uses a positive sign after an even left coproduct factor" $
      perturbation `onBasis` word [EvenOdd]
        `shouldBe` singleComb (word [EvenPrimitive, OddPrimitive])

    it "uses a negative sign after an odd left coproduct factor" $
      perturbation `onBasis` word [OddEven]
        `shouldBe` -(singleComb (word [OddPrimitive, EvenPrimitive]))

  it "uses the finite part of each second-quadrant total diagonal" $
    totalBidegrees (cobarTensor SignCoalgebra) 3
      `shouldBe` [(0, 3), (-1, 4), (-2, 5), (-3, 6)]

  it "restricts Cobar to the coaugmentation coideal" $
    isBasis (Cobar SignCoalgebra) [SignUnit] `shouldBe` False

  describe "on chains of S^2 x S^2" $ do
    let cobar = Cobar (NChains (Product (Sphere 2) (Sphere 2)))

    describe "is a bicomplex" $ do
      let as = do
            horizontalDegree <- [-5 .. 0]
            verticalDegree <- [0 .. 10]
            bibasis cobar (horizontalDegree, verticalDegree)
      BicomplexProperties.checkChainConditions cobar as

    ChainComplexProperties.checkChainCondition
      cobar
      5

  describe "algebra structure" $
    AlgebraProperties.check 5 (Cobar SignCoalgebra)

  describe "augmentation" $
    AlgebraProperties.checkAugmented 5 (Cobar SignCoalgebra)

  describe "Omega S^2" $
    it "has one infinite-cyclic homology generator in every degree" $
      take 7 (homologies (Cobar (NChains (Sphere 2))))
        `shouldBe` replicate 7 (freeAbGroup 1)

  describe "Omega S^3" $
    it "has one infinite-cyclic homology generator in every even degree" $
      take 8 (homologies (Cobar (NChains (Sphere 3))))
        `shouldBe` [freeAbGroup (if even n then 1 else 0) | n <- [0 :: Int .. 7]]
