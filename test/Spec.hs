import Test.Hspec (hspec, describe)
import Data.Proxy

import Math.Algebra.AbGroup
import Math.ValueCategory.Abelian.Cached

import qualified SmithNormalFormTest
import qualified MatrixOpsTest
import qualified AbGroupTest
import qualified AbelianCategoryProperties

main :: IO ()
main = hspec spec

spec = do
  SmithNormalFormTest.spec
  MatrixOpsTest.spec

  describe "Abelian category problems for AbGroup" $ AbelianCategoryProperties.spec (Proxy :: Proxy AbGroup)
  describe "Abelian category problems for Cached AbGroup" $ AbelianCategoryProperties.spec (Proxy :: Proxy (Cached AbGroup))
  AbGroupTest.spec
