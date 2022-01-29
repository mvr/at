import Test.Hspec
import Data.Proxy

import Math.Algebra.AbGroupPres

import qualified SmithNormalFormTest
import qualified MatrixOpsTest
import qualified AbGroupPresTest
import qualified AbelianCategoryProperties

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  SmithNormalFormTest.spec
  MatrixOpsTest.spec

  describe "Abelian category problems for AbGroup" $ AbelianCategoryProperties.spec (Proxy @AbGroupPres)
  -- describe "Abelian category problems for Cached AbGroup" $ AbelianCategoryProperties.spec (Proxy @AbGroupPres)
  AbGroupPresTest.spec
