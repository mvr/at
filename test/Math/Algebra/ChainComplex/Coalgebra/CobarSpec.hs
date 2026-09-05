module Math.Algebra.ChainComplex.Coalgebra.CobarSpec where

import Control.Category.Constrained (id, (.))
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Test.Hspec
import Prelude hiding (id, (.))

import Math.Algebra.AbGroupPres (freeAbGroup)
import Math.Algebra.Bicomplex hiding (FiniteType)
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Coalgebra
import Math.Algebra.ChainComplex.Coalgebra.Cobar
import Math.Algebra.ChainComplex.Disk
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.Combination
import Math.Topology.SSet.NChains
import Math.Topology.SSet.Product
import Math.Topology.SSet.Sphere

import qualified Math.Algebra.Bicomplex.Properties as BicomplexProperties
import qualified Math.Algebra.ChainComplex.Algebra.Properties as AlgebraProperties
import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties
import qualified Math.Algebra.ChainComplex.Reduction.Properties as ReductionProperties

data SignCoalgebra = SignCoalgebra

data SignBasis
  = SignUnit
  | EvenPrimitive
  | OddPrimitive
  | DesuspensionBoundary
  | DesuspensionInterior
  | EvenOdd
  | OddEven
  | BoundaryEven
  | EvenBoundary
  deriving (Eq, Ord, Show)

instance ChainComplex SignCoalgebra where
  type Basis SignCoalgebra = SignBasis

  degree _ SignUnit = 0
  degree _ EvenPrimitive = 2
  degree _ OddPrimitive = 3
  degree _ DesuspensionBoundary = 3
  degree _ DesuspensionInterior = 4
  degree _ EvenOdd = 5
  degree _ OddEven = 5
  degree _ BoundaryEven = 5
  degree _ EvenBoundary = 5

  diff _ = Morphism (-1) $ \b -> case b of
    DesuspensionInterior -> singleComb DesuspensionBoundary
    _ -> 0

instance BoundedBelow SignCoalgebra where
  lowerBound _ = 0

instance ConnectedChainComplex SignCoalgebra

instance OneReducedChainComplex SignCoalgebra

instance FiniteType SignCoalgebra where
  basis _ 0 = [SignUnit]
  basis _ 2 = [EvenPrimitive]
  basis _ 3 = [OddPrimitive, DesuspensionBoundary]
  basis _ 4 = [DesuspensionInterior]
  basis _ 5 = [EvenOdd, OddEven, BoundaryEven, EvenBoundary]
  basis _ _ = []

instance Coalgebra SignCoalgebra where
  counitMor _ = Morphism 0 $ \b ->
    if b == SignUnit then singleComb () else 0

  delMor _ = Morphism 0 $ \b -> case b of
    SignUnit -> singleComb (SignUnit, SignUnit)
    EvenPrimitive -> primitive EvenPrimitive
    OddPrimitive -> primitive OddPrimitive
    DesuspensionBoundary -> primitive DesuspensionBoundary
    DesuspensionInterior ->
      primitive DesuspensionInterior
        + singleComb (EvenPrimitive, EvenPrimitive)
    EvenOdd -> primitive EvenOdd + singleComb (EvenPrimitive, OddPrimitive)
    OddEven -> primitive OddEven + singleComb (OddPrimitive, EvenPrimitive)
    -- Eliminating the boundary/interior pair produces higher Cobar terms.
    BoundaryEven -> primitive BoundaryEven + singleComb (DesuspensionBoundary, EvenPrimitive)
    EvenBoundary -> primitive EvenBoundary + singleComb (EvenPrimitive, DesuspensionBoundary)
    where
      primitive b = singleComb (SignUnit, b) + singleComb (b, SignUnit)

instance CoaugmentedCoalgebra SignCoalgebra where
  coaugmentationMor _ = Morphism 0 $ const $ singleComb SignUnit

data SignModel = SignModel

instance ChainComplex SignModel where
  type Basis SignModel = SignBasis

  isBasis _ b =
    b /= DesuspensionBoundary
      && b /= DesuspensionInterior
      && isBasis SignCoalgebra b
  degree _ = degree SignCoalgebra
  diff _ = morphismZeroOfDeg (-1)

instance BoundedBelow SignModel where
  lowerBound _ = 0

instance ConnectedChainComplex SignModel

instance OneReducedChainComplex SignModel

instance FiniteType SignModel where
  basis _ d = filter (isBasis SignModel) (basis SignCoalgebra d)
spec :: Spec
spec = describe "Cobar" $ do
  describe "cobarFunc" $ do
    let c = SignCoalgebra
        a = Cobar c
        bs = [0 .. 5] >>= basis c
        ws = [0 .. 6] >>= basis a
        -- The weights respect both the differential and all coproduct terms.
        scale s t = Morphism 0 $ \b ->
          let k = case b of
                SignUnit -> 1
                EvenPrimitive -> s
                OddPrimitive -> t
                DesuspensionBoundary -> s * s
                DesuspensionInterior -> s * s
                EvenOdd -> s * t
                OddEven -> s * t
                BoundaryEven -> s * s * s
                EvenBoundary -> s * s * s
           in k .* singleComb b
        f, g :: Morphism SignCoalgebra SignCoalgebra
        f = scale 2 3
        g = scale (-1) 2
        tf = cobarFunc f

    it "uses coaugmented coalgebra maps as inputs" $
      forM_ [f, g] $ \h -> do
        (delMor c . h, tensorFunc c c h h . delMor c)
          `ChainComplexProperties.isEqOnAll` bs
        (h . coaugmentationMor c, coaugmentationMor c)
          `ChainComplexProperties.isEqOnAll` [()]
        (counitMor c . h, counitMor c)
          `ChainComplexProperties.isEqOnAll` bs
        (diff c . h, h . diff c)
          `ChainComplexProperties.isEqOnAll` bs

    it "preserves identity" $
      cobarFunc (id :: Morphism SignCoalgebra SignCoalgebra)
        `ChainComplexProperties.isIdOnAll` ws

    it "preserves composition" $
      (cobarFunc (g . f), cobarFunc g . tf)
        `ChainComplexProperties.isEqOnAll` ws

    it "preserves degree" $ do
      morphismDegree tf `shouldBe` 0
      forM_ ws $ \w ->
        forM_ (coeffs (tf `onBasis` w)) $ \(_, v) ->
          degree a v `shouldBe` degree a w

    it "applies the map to each letter in order" $
      tf `onBasis` [EvenPrimitive, OddPrimitive]
        `shouldBe` 6 .* singleComb [EvenPrimitive, OddPrimitive]

    it "preserves the unit and augmentation" $ do
      (tf . unitMor a, unitMor a)
        `ChainComplexProperties.isEqOnAll` [()]
      (augmentationMor a . tf, augmentationMor a)
        `ChainComplexProperties.isEqOnAll` ws

    it "preserves multiplication" $
      (tf . muMor a, muMor a . tensorFunc a a tf tf)
        `ChainComplexProperties.isEqOnAll` ([0 .. 6] >>= basis (Tensor a a))

    it "rejects nonzero degrees through the tensor-algebra check" $
      forM_ [-2, -1, 1, 2] $ \d ->
        evaluate (cobarFunc (morphismZeroOfDeg d :: Morphism SignCoalgebra SignCoalgebra))
          `shouldThrow` errorCall "tensorAlgebraFunc: expected a degree-zero morphism"

    ChainComplexProperties.checkChainMap a a "cobarFunc" ws tf

  describe "Cobar tensor truncation" $ do
    let t = CobarTensor (Disk 2)

    ChainComplexProperties.checkChainConditionOn t [[], [DiskInterior], [DiskInterior, DiskInterior]]

    it "discards degree-one boundaries" $
      diff t `onBasis` [DiskInterior] `shouldBe` zeroCombination

    it "excludes degree-zero and degree-one generators" $
      forM_ [DiskBase, DiskBoundary] $ \b ->
        isBasis t [b] `shouldBe` False

    it "enumerates words in the desuspended degree-two generator" $ do
      degree t [DiskInterior] `shouldBe` 1
      basis t 0 `shouldBe` [[]]
      basis t 1 `shouldBe` [[DiskInterior]]
      basis t 2 `shouldBe` [[DiskInterior, DiskInterior]]

    it "has lower bound zero without requiring a one-reduced input" $
      lowerBound t `shouldBe` 0

  it "bounds Cobar words below by zero, including the empty word" $ do
    lowerBound (CobarTensor SignCoalgebra) `shouldBe` 0
    lowerBound (Cobar SignCoalgebra) `shouldBe` 0
    basis (Cobar SignCoalgebra) 0 `shouldBe` [[]]

  describe "Cobar tensor reduction" $
    ReductionProperties.check
      8
      (CobarTensor (Disk 3))
      (CobarTensor ())
      (cobarTensorReduction (Disk 3) (diskReduction (Disk 3)))

  describe "desuspension signs" $ do
    let perturbation = cobarPerturbation SignCoalgebra
        word = id

    it "negates the internal differential on a desuspended generator" $
      diff (CobarTensor SignCoalgebra) `onBasis` word [DesuspensionInterior]
        `shouldBe` -(singleComb (word [DesuspensionBoundary]))

    it "uses a positive sign after an even left coproduct factor" $
      perturbation `onBasis` word [EvenOdd]
        `shouldBe` singleComb (word [EvenPrimitive, OddPrimitive])

    it "uses a negative sign after an odd left coproduct factor" $
      perturbation `onBasis` word [OddEven]
        `shouldBe` -(singleComb (word [OddPrimitive, EvenPrimitive]))

  it "uses the finite part of each second-quadrant total diagonal" $
    totalBidegrees (CobarTensor SignCoalgebra) 3
      `shouldBe` [(0, 3), (-1, 4), (-2, 5), (-3, 6)]

  describe "word-length filtration" $ do
    let tensor = CobarTensor SignCoalgebra
        perturbation = cobarPerturbation SignCoalgebra
        words = [0 .. 6] >>= basis tensor

    it "bounds word length by total degree" $
      forM_ [0 .. 6] $ \totalDegree ->
        forM_ (basis tensor totalDegree) $ \word ->
          length word `shouldSatisfy` (<= totalDegree)

    it "is raised by the coproduct perturbation" $
      forM_ words $ \word ->
        forM_ (coeffs (perturbation `onBasis` word)) $ \(_, image) ->
          length image `shouldBe` length word + 1

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

    ChainComplexProperties.checkChainCondition cobar 5

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
        `shouldBe` (\n -> freeAbGroup (if even n then 1 else 0)) <$> [0 :: Int .. 7]
