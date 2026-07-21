module Main where

import Test.Hspec

import qualified Math.Algebra.AbGroupPres.MatrixOpsSpec
import qualified Math.Algebra.AbGroupPresSpec
import qualified Math.Algebra.ChainComplex.Algebra.BarSpec
import qualified Math.Algebra.ChainComplex.BiconeSpec
import qualified Math.Algebra.ChainComplex.DiskSpec
import qualified Math.Algebra.ChainComplex.FundamentalCocycleSpec
import qualified Math.Algebra.ChainComplex.HomSpec
import qualified Math.Algebra.ChainComplex.ShiftSpec
import qualified Math.Algebra.ChainComplex.TensorSpec
import qualified Math.Algebra.GroupSpec
import qualified Math.Algebra.SmithNormalFormSpec
import qualified Math.Topology.SGrp.KGnSpec
import qualified Math.Topology.SGrp.WbarDiscreteSpec
import qualified Math.Topology.SGrp.WbarSpec
import qualified Math.Topology.SSetSpec
import qualified Math.Topology.SSet.MooreSpec
import qualified Math.Topology.SSet.ProductSpec
import qualified Math.Topology.SSet.RPnSpec
import qualified Math.Topology.SSet.SkeletonSpec
import qualified Math.Topology.SSet.SphereSpec
import qualified Math.Topology.SSet.TwistedProductSpec
import qualified Math.Topology.SSet.WhiteheadSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Math.Algebra.SmithNormalFormSpec.spec
  Math.Algebra.ChainComplex.FundamentalCocycleSpec.spec
  Math.Algebra.GroupSpec.spec
  Math.Topology.SSetSpec.spec
  Math.Topology.SSet.WhiteheadSpec.spec
  Math.Topology.SGrp.KGnSpec.spec
  Math.Algebra.ChainComplex.BiconeSpec.spec
  Math.Algebra.ChainComplex.DiskSpec.spec
  Math.Algebra.ChainComplex.TensorSpec.spec
  Math.Algebra.ChainComplex.Algebra.BarSpec.spec
  Math.Algebra.ChainComplex.ShiftSpec.spec
  Math.Algebra.AbGroupPres.MatrixOpsSpec.spec
  Math.Algebra.AbGroupPresSpec.spec
  Math.Algebra.ChainComplex.HomSpec.spec
  Math.Topology.SSet.ProductSpec.spec
  Math.Topology.SSet.SphereSpec.spec
  Math.Topology.SSet.MooreSpec.spec
  Math.Topology.SSet.SkeletonSpec.spec
  Math.Topology.SSet.RPnSpec.spec
  Math.Topology.SGrp.WbarDiscreteSpec.spec
  Math.Topology.SGrp.WbarSpec.spec
  Math.Topology.SSet.TwistedProductSpec.spec
