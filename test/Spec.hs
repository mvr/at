import Test.Hspec (hspec)
import qualified SmithNormalFormTest

main :: IO ()
main = hspec spec

spec = do
  SmithNormalFormTest.spec
