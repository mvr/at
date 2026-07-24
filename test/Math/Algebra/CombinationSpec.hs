module Math.Algebra.CombinationSpec where

import Data.Function (on)
import Data.List (groupBy, sortOn)
import Test.Hspec
import Test.QuickCheck

import Math.Algebra.Combination

spec :: Spec
spec = describe "Combination" $ do
  it "stores terms in canonical form" $ do
    coeffs (fromTerms [(2, 'b'), (1, 'a'), (-2, 'b'), (3, 'a'), (0, 'c')])
      `shouldBe` [(4, 'a')]

  it "normalises arbitrary terms in ascending order" $
    property $ \(terms :: [(Int, Int)]) ->
      normalise terms == referenceNormalise terms

  it "linear addition agrees with normalising all terms together" $
    property $ \(left :: [(Int, Int)]) right ->
      coeffs (fromTerms left + fromTerms right) == normalise (left ++ right)

  it "combines basis elements identified by a map" $ do
    let combination = fromTerms [(1, 1), (2, 3), (4, 2)] :: Combination Int
    coeffs (mapCombination (`mod` 2) combination)
      `shouldBe` [(4, 0), (3, 1)]

  it "multiplies coefficients and combines terms when binding" $ do
    let outer = fromTerms [(2, 'a'), (-1, 'b')]
        inner :: Char -> Combination Int
        inner 'a' = fromTerms [(1, 0), (1, 1)]
        inner _ = fromTerms [(2, 0)]
    coeffs (bindCombination outer inner)
      `shouldBe` [(2, 1)]

  it "binds many overlapping images like one-shot normalisation" $ do
    let outer = fromTerms [(coefficient, basis) | basis <- [0 .. 127], let coefficient = basis `mod` 5 - 2]
        inner basis = fromTerms [(1, basis `mod` 7), (-1, (basis + 1) `mod` 7)]
        expected =
          fromTerms
            [ (outerCoefficient * innerCoefficient, innerBasis)
              | (outerCoefficient, outerBasis) <- coeffs outer,
                (innerCoefficient, innerBasis) <- coeffs (inner outerBasis)
            ]
    bindCombination outer inner `shouldBe` expected

  it "forms products in canonical pair order" $ do
    let left = fromTerms [(2, 'b'), (1, 'a')]
        right = fromTerms [(3, 2), (4, 1)] :: Combination Int
    coeffs (productCombination left right)
      `shouldBe` [(4, ('a', 1)), (3, ('a', 2)), (8, ('b', 1)), (6, ('b', 2))]

  it "looks up coefficients in the ordered representation" $ do
    let combination = fromTerms [(2, 1), (4, 3)] :: Combination Int
    map (coeffOf combination) [0 .. 4]
      `shouldBe` [0, 2, 0, 4, 0]

referenceNormalise :: [(Int, Int)] -> [(Int, Int)]
referenceNormalise terms =
  concatMap combineGroup (groupBy ((==) `on` snd) (sortOn snd terms))
  where
    combineGroup group@((_, basis) : _)
      | coefficient == 0 = []
      | otherwise = [(coefficient, basis)]
      where
        coefficient = sum (fmap fst group)
    combineGroup [] = []
