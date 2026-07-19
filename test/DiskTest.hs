module DiskTest where

import Test.Hspec

import Math.Algebra.ChainComplex.Disk

import qualified ReductionProperties

spec :: Spec
spec = describe "disk chain complexes" $ do
  describe "Disk 1" $
    ReductionProperties.check 1 (Disk 1) () (diskReduction (Disk 1))

  describe "Disk 2" $
    ReductionProperties.check 2 (Disk 2) () (diskReduction (Disk 2))
