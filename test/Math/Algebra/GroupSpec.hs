module Math.Algebra.GroupSpec where

import Test.Hspec

import Math.Algebra.Group

spec :: Spec
spec = do
  describe "power" $ do
    it "raises integral group elements to integer powers" $
      power Z (-3) 4 `shouldBe` (-12)

    it "raises modular group elements to integer powers" $ do
      let coefficients = Zmod 5
      power coefficients (-3) (zmodElement coefficients (2 :: Integer))
        `shouldBe` zmodElement coefficients (4 :: Integer)

  describe "normaliseGroupTerms" $ do
    it "sorts terms, combines repeated keys, and removes units" $
      normaliseGroupTerms
        Z
        [ (2 :: Int, 3 :: Integer),
          (1, 4),
          (2, -1),
          (3, 0),
          (2, -2)
        ]
        `shouldBe` [(1, 4)]

    it "combines values using the supplied coefficient group" $ do
      let coefficients = Zmod 5
          value = zmodElement coefficients
      normaliseGroupTerms
        coefficients
        [ ('b', value (1 :: Integer)),
          ('a', value (2 :: Integer)),
          ('b', value (4 :: Integer))
        ]
        `shouldBe` [('a', value (2 :: Integer))]
