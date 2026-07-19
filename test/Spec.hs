module Main where

import Test.Hspec

import qualified Math.Algebra.AbGroupPres.MatrixOpsSpec
import qualified Math.Algebra.AbGroupPresSpec
import qualified Math.Algebra.ChainComplex.Algebra.BarSpec
import qualified Math.Algebra.ChainComplex.DiskSpec
import qualified Math.Algebra.ChainComplex.HomSpec
import qualified Math.Algebra.ChainComplex.ShiftSpec
import qualified Math.Algebra.ChainComplex.TensorSpec
import qualified Math.Algebra.SmithNormalFormSpec
import qualified Math.Topology.SGrp.KGnSpec
import qualified Math.Topology.SGrp.WbarDiscreteSpec
import qualified Math.Topology.SGrp.WbarSpec
import qualified Math.Topology.SSet.ProductSpec
import qualified Math.Topology.SSet.TwistedProductSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Math.Algebra.SmithNormalFormSpec.spec
  Math.Topology.SGrp.KGnSpec.spec
  Math.Algebra.ChainComplex.DiskSpec.spec
  Math.Algebra.ChainComplex.TensorSpec.spec
  Math.Algebra.ChainComplex.Algebra.BarSpec.spec
  Math.Algebra.ChainComplex.ShiftSpec.spec
  Math.Algebra.AbGroupPres.MatrixOpsSpec.spec
  Math.Algebra.AbGroupPresSpec.spec
  Math.Algebra.ChainComplex.HomSpec.spec
  Math.Topology.SSet.ProductSpec.spec
  Math.Topology.SGrp.WbarDiscreteSpec.spec
  Math.Topology.SGrp.WbarSpec.spec
  Math.Topology.SSet.TwistedProductSpec.spec
