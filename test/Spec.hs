import Test.Hspec (hspec)
import qualified SmithNormalFormTest
import qualified MatrixOpsTest
import qualified KernelCokernelTest

main :: IO ()
main = hspec spec

spec = do
  SmithNormalFormTest.spec
  MatrixOpsTest.spec
  KernelCokernelTest.spec
