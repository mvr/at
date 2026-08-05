module Math.Algebra.Group.FreeSpec where

import Test.Hspec

import Math.Algebra.Group.Free

spec :: Spec
spec = describe "reduced free-group words" $ do
  it "compresses adjacent powers" $
    wordPowers (reducedWord [('a', 2), ('a', -1), ('b', 3)])
      `shouldBe` [('a', 1), ('b', 3)]

  it "continues reducing after cancellation" $
    reducedWord [('a', 1), ('b', 2), ('b', -2), ('a', -1)]
      `shouldBe` (mempty :: FreeWord Char)

  it "inverts words in reverse order" $
    wordPowers (inverseWord (reducedWord [('a', 2), ('b', -3)]))
      `shouldBe` [('b', 3), ('a', -2)]

  it "substitutes powered words noncommutatively" $
    wordPowers
      ( bindWord
          (reducedWord [('a', 2)])
          (const (reducedWord [('b', 1), ('c', 1)]))
      )
      `shouldBe` [('b', 1), ('c', 1), ('b', 1), ('c', 1)]
