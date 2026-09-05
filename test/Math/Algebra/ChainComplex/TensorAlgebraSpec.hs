module Math.Algebra.ChainComplex.TensorAlgebraSpec where

import Control.Category.Constrained (id, (.))
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM)
import Test.Hspec
import Prelude hiding (id, (.))

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Disk
import Math.Algebra.ChainComplex.Shift
import Math.Algebra.ChainComplex.TensorAlgebra
import Math.Algebra.ChainComplex.Truncation
import Math.Algebra.Combination

import qualified Math.Algebra.ChainComplex.Properties as ChainComplexProperties
import qualified Math.Algebra.ChainComplex.Reduction.Properties as ReductionProperties

spec :: Spec
spec = describe "tensor algebra" $ do
  let top = TensorAlgebra (Disk 2)
      bottom = TensorAlgebra ()
      topBasis =
        [ [],
          [DiskBase],
          [DiskBoundary],
          [DiskInterior],
          [DiskBoundary, DiskInterior],
          [DiskInterior, DiskBoundary]
        ]
      bottomBasis = [[], [()], [(), ()]]

  ChainComplexProperties.checkChainConditionOn top topBasis

  describe "tensorAlgebraFunc" $ do
    let a = TensorAlgebra (Disk 1)
        ws = do
          l <- [0 .. 3]
          replicateM l [DiskBase, DiskBoundary, DiskInterior]
        f, g :: Morphism Disk Disk
        f = Morphism 0 $ \case
          DiskBase -> singleComb DiskBase + singleComb DiskBoundary
          b -> singleComb b
        g = Morphism 0 $ \case
          DiskBase -> singleComb DiskBase
          b -> 2 .* singleComb b
        tf = tensorAlgebraFunc f

    it "preserves identity" $
      tensorAlgebraFunc (id :: Morphism Disk Disk)
        `ChainComplexProperties.isIdOnAll` ws

    it "preserves composition" $
      (tensorAlgebraFunc (g . f), tensorAlgebraFunc g . tf)
        `ChainComplexProperties.isEqOnAll` ws

    it "preserves degree" $ do
      morphismDegree tf `shouldBe` 0
      forM_ ws $ \w ->
        forM_ (coeffs (tf `onBasis` w)) $ \(_, v) ->
          degree a v `shouldBe` degree a w

    it "extends generator images multiplicatively" $
      tf `onBasis` [DiskBase, DiskInterior]
        `shouldBe` singleComb [DiskBase, DiskInterior]
          + singleComb [DiskBoundary, DiskInterior]

    it "preserves the empty word even for the zero map" $
      tensorAlgebraFunc (morphismZeroOfDeg 0 :: Morphism Disk Disk) `onBasis` []
        `shouldBe` singleComb []

    it "rejects positive and negative degrees before applying the map" $
      forM_ [-2, -1, 1, 2] $ \d ->
        evaluate (tensorAlgebraFunc (morphismZeroOfDeg d :: Morphism Disk Disk))
          `shouldThrow` errorCall "tensorAlgebraFunc: expected a degree-zero morphism"

    ChainComplexProperties.checkChainMap a a "tensorAlgebraFunc" ws tf

  describe "tensorWords" $ do
    it "includes the empty word only in degree zero and length zero" $ do
      tensorWords () 0 0 `shouldBe` [[]]
      tensorWords () 1 0 `shouldBe` []
      tensorWords () (-1) 0 `shouldBe` []

    it "rejects negative lengths" $
      tensorWords (Desusp ()) 1 (-1) `shouldBe` []

    it "enumerates fixed-length words in degree-zero generators" $ do
      tensorWords () 0 3 `shouldBe` [[(), (), ()]]
      tensorWords () 1 3 `shouldBe` []

    it "rejects degrees below the length times the lower bound" $ do
      tensorWords (Susp ()) 2 3 `shouldBe` []
      tensorWords (Desusp ()) (-4) 3 `shouldBe` []

    it "enumerates every word once with positive, zero, or negative bounds" $
      forM_ [-2 .. 2] $ \k -> do
        let a = Shift k (Disk 2)
        forM_ [0 .. 3] $ \l ->
          forM_ [-7 .. 13] $ \d -> do
            let ws = replicateM l [DiskBase, DiskBoundary, DiskInterior]
            tensorWords a d l
              `shouldMatchList` filter ((== d) . degree (TensorAlgebra a)) ws

    it "does not require a sharp lower bound" $ do
      let a = NaiveTruncation (-2) ()
      tensorWords a 0 3 `shouldBe` [[(), (), ()]]
      tensorWords a (-1) 3 `shouldBe` []

  describe "on a suspension" $ do
    let tensor = TensorAlgebra (Susp (Disk 2))

    ChainComplexProperties.checkChainConditionOn
      tensor
      [[], [DiskBase], [DiskBoundary], [DiskInterior], [DiskInterior, DiskInterior]]

    it "suspends degree-zero generators" $
      degree tensor [DiskBase] `shouldBe` 1

    it "negates their internal differential" $
      diff tensor `onBasis` [DiskInterior]
        `shouldBe` -(singleComb [DiskBoundary])

  describe "on a desuspension" $ do
    let tensor = TensorAlgebra (Desusp (Disk 1))

    ChainComplexProperties.checkChainConditionOn
      tensor
      [[], [DiskBase], [DiskBoundary], [DiskInterior], [DiskInterior, DiskInterior]]

    it "allows degree-one generators" $
      degree tensor [DiskInterior] `shouldBe` 0

    it "retains their internal differential" $
      diff tensor `onBasis` [DiskInterior]
        `shouldBe` -(singleComb [DiskBoundary])

  describe "tensor-trick reduction" $
    ReductionProperties.checkOn
      top
      bottom
      topBasis
      bottomBasis
      (tensorAlgebraReduction (Disk 2) (diskReduction (Disk 2)))
