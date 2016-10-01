import Test.Hspec (hspec)
import qualified SmithNormalFormTest
import qualified KernelCokernelTest

main :: IO ()
main = hspec spec

spec = do
  SmithNormalFormTest.spec
  KernelCokernelTest.spec
