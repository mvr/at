module Math.Algebra.Combination
  ( Combination,
    coeffs,
    fromTerms,
    zeroCombination,
    coeffOf,
    mapCombination,
    mapMonotonic,
    bindCombination,
    liftCombination2,
    productCombination,
    traverseCombination,
    (.*),
    singleComb,
    normalise,
  )
where

import Control.Category.Constrained (join, return)
import qualified Control.Category.Constrained as Constrained
import Prelude hiding (id, return, (.))

-- | Z-linear combinations, stored in ascending basis order without zero terms.
newtype Combination b = CanonicalCombination {coeffs :: [(Int, b)]}
  deriving (Eq)

fromTerms :: Ord b => [(Int, b)] -> Combination b
fromTerms terms = CanonicalCombination (normalise terms)

zeroCombination :: Combination b
zeroCombination = CanonicalCombination []

showAddTerm :: Show b => (Int, b) -> String
showAddTerm (0, _) = error "showTerm: 0 coefficient"
showAddTerm (1, b) = show b
showAddTerm (-1, b) = " - " ++ show b
showAddTerm (c, b)
  | c < 0 = " - " ++ show (-c) ++ "·" ++ show b
  | otherwise = " + " ++ show c ++ "·" ++ show b

showSoloTerm :: Show b => (Int, b) -> String
showSoloTerm (0, _) = error "showSoloTerm: 0 coefficient"
showSoloTerm (1, b) = show b
showSoloTerm (-1, b) = "-" ++ show b
showSoloTerm (c, b) = show c ++ "·" ++ show b

instance Show b => Show (Combination b) where
  show (CanonicalCombination []) = "0"
  show (CanonicalCombination [t]) = showSoloTerm t
  show (CanonicalCombination (t : cs)) = show (CanonicalCombination cs) ++ showAddTerm t

coeffOf :: Ord b => Combination b -> b -> Int
coeffOf (CanonicalCombination terms) target = go terms
  where
    go [] = 0
    go ((coefficient, basis) : rest) = case compare target basis of
      LT -> 0
      EQ -> coefficient
      GT -> go rest

normalise :: (Ord b, Num a, Eq a) => [(a, b)] -> [(a, b)]
normalise [] = []
normalise [(coefficient, basis)]
  | coefficient == 0 = []
  | otherwise = [(coefficient, basis)]
normalise terms = foldl' insertTerm [] terms
  where
    insertTerm canonical (coefficient, basis)
      | coefficient == 0 = canonical
      | otherwise = insert canonical
      where
        insert [] = [(coefficient, basis)]
        insert existing@((existingCoefficient, existingBasis) : rest) =
          case compare basis existingBasis of
            LT -> (coefficient, basis) : existing
            EQ
              | total == 0 -> rest
              | otherwise -> (total, basis) : rest
              where
                total = coefficient + existingCoefficient
            GT -> (existingCoefficient, existingBasis) : insert rest

mapCombination :: Ord b => (a -> b) -> Combination a -> Combination b
mapCombination f (CanonicalCombination terms) =
  fromTerms (fmap (fmap f) terms)

-- | Map a strictly monotonic function over a canonical combination.
-- The caller must ensure that the function preserves the basis ordering.
mapMonotonic :: (a -> b) -> Combination a -> Combination b
mapMonotonic f (CanonicalCombination terms) =
  CanonicalCombination (fmap (fmap f) terms)

bindCombination :: Ord b => Combination a -> (a -> Combination b) -> Combination b
bindCombination (CanonicalCombination []) _ = zeroCombination
bindCombination (CanonicalCombination [(coefficient, basis)]) f = coefficient .* f basis
bindCombination (CanonicalCombination [(leftCoefficient, leftBasis), (rightCoefficient, rightBasis)]) f =
  leftCoefficient .* f leftBasis + rightCoefficient .* f rightBasis
bindCombination (CanonicalCombination [first, second, third]) f =
  apply first + apply second + apply third
  where
    apply (coefficient, basis) = coefficient .* f basis
bindCombination (CanonicalCombination terms) f =
  sumCombinations
    [ outerCoefficient .* f outerBasis
      | (outerCoefficient, outerBasis) <- terms
    ]

sumCombinations :: Ord b => [Combination b] -> Combination b
sumCombinations [] = zeroCombination
sumCombinations [combination] = combination
sumCombinations combinations = sumCombinations (mergePairs combinations)
  where
    mergePairs (left : right : rest) = left + right : mergePairs rest
    mergePairs rest = rest

liftCombination2 :: Ord c => (a -> b -> c) -> Combination a -> Combination b -> Combination c
liftCombination2 f left right =
  fromTerms
    [ (leftCoefficient * rightCoefficient, f leftBasis rightBasis)
      | (leftCoefficient, leftBasis) <- coeffs left,
        (rightCoefficient, rightBasis) <- coeffs right
    ]

-- | Pair every term in two combinations. Lexicographic pair ordering
-- preserves the canonical ordering of the inputs.
productCombination :: Combination a -> Combination b -> Combination (a, b)
productCombination (CanonicalCombination left) (CanonicalCombination right) =
  CanonicalCombination
    [ (leftCoefficient * rightCoefficient, (leftBasis, rightBasis))
      | (leftCoefficient, leftBasis) <- left,
        (rightCoefficient, rightBasis) <- right
    ]

traverseCombination :: Ord b => (a -> Combination b) -> [a] -> Combination [b]
traverseCombination f = foldr (\a rest -> liftCombination2 (:) (f a) rest) (singleComb [])

(.*) :: Int -> Combination b -> Combination b
0 .* _ = zeroCombination
1 .* combination = combination
n .* (CanonicalCombination terms) =
  CanonicalCombination $ fmap (\(coefficient, basis) -> (n * coefficient, basis)) terms

infixl 7 .*

singleComb :: b -> Combination b
singleComb a = CanonicalCombination [(1, a)]

instance Constrained.Functor (Constrained.Sub Ord (->)) (->) Combination where
  fmap (Constrained.Sub f) = mapCombination f

instance Constrained.Functor (Constrained.Sub Ord (->)) (Constrained.Sub Ord (->)) Combination where
  fmap f = Constrained.Sub $ Constrained.fmap f

instance Constrained.Monad (Constrained.Sub Ord (->)) Combination where
  return = Constrained.Sub singleComb
  join = Constrained.Sub $ \combinations -> bindCombination combinations (\combination -> combination)

instance Ord b => Num (Combination b) where
  fromInteger 0 = zeroCombination
  fromInteger _ = error "Combination: fromInteger"

  CanonicalCombination left + CanonicalCombination right =
    CanonicalCombination (merge left right)
    where
      merge [] terms = terms
      merge terms [] = terms
      merge leftTerms@((leftCoefficient, leftBasis) : leftRest) rightTerms@((rightCoefficient, rightBasis) : rightRest) =
        case compare leftBasis rightBasis of
          LT -> (leftCoefficient, leftBasis) : merge leftRest rightTerms
          GT -> (rightCoefficient, rightBasis) : merge leftTerms rightRest
          EQ -> case leftCoefficient + rightCoefficient of
            0 -> merge leftRest rightRest
            coefficient -> (coefficient, leftBasis) : merge leftRest rightRest

  negate (CanonicalCombination terms) =
    CanonicalCombination $ fmap (\(coefficient, basis) -> (negate coefficient, basis)) terms

  (*) = error "Combination: (*)"
  abs = error "Combination: abs"
  signum = error "Combination: signum"
