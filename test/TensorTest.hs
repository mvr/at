module TensorTest where

import Control.Category.Constrained (id)
import Control.Monad (forM_)
import Prelude hiding (id)
import Test.Hspec

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Disk
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.Combination

import qualified ReductionProperties

spec :: Spec
spec = describe "tensor products" $ do
  describe "tensorFunc" $ do
    it "preserves identity morphisms" $ do
      let disk = Disk 2
          tensorId = tensorFunc disk disk id id
      forM_ ([0 .. 4] >>= basis (Tensor disk disk)) $ \b ->
        tensorId `onBasis` b `shouldBe` singleComb b

    it "uses the second morphism's degree in the Koszul rule" $ do
      let raise = Morphism 1 $ \case
            DiskBase -> singleComb DiskBoundary
            _ -> 0
          disk = Disk 2
          tensorRaise = tensorFunc disk disk id raise
      morphismDegree tensorRaise `shouldBe` 1
      tensorRaise `onBasis` (DiskBoundary, DiskBase)
        `shouldBe` -(singleComb (DiskBoundary, DiskBoundary))

  describe "tensorReduction" $
    ReductionProperties.check
      4
      (Tensor (Disk 2) (Disk 2))
      (Tensor () ())
      ( tensorReduction
          (Disk 2)
          (Disk 2)
          ()
          ()
          (diskReduction (Disk 2))
          (diskReduction (Disk 2))
      )
