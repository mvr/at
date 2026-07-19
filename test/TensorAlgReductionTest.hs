module TensorAlgReductionTest where

import Test.Hspec

import Math.Algebra.ChainComplex.Algebra.Bar
import Math.Algebra.ChainComplex.Disk

import qualified ReductionProperties

spec :: Spec
spec = describe "tensor algebra reduction signs" $
  ReductionProperties.check
    10
    (TensorSusp (Disk 2))
    (TensorSusp ())
    (tensorAlgReduction (Disk 2) () (diskReduction (Disk 2)))
