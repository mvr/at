module Math.Algebra.GroupSpec where

import Test.Hspec

import Math.Algebra.Group

spec :: Spec
spec = describe "Zmod" $
  it "reduces sums beyond the Int range without overflow" $ do
    let modulus = fromIntegral (maxBound :: Int) + 2
        nearTop = ZmodElement (modulus - 1)
    prod (Zmod modulus) nearTop nearTop
      `shouldBe` ZmodElement (modulus - 2)
