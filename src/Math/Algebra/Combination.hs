module Math.Algebra.Combination where

import Control.Category.Constrained (join, return)
import qualified Control.Category.Constrained as Constrained
import Data.List (find)
import Data.Maybe (fromMaybe)
import Prelude hiding (id, return, (.))

-- | Z-linear combinations
newtype Combination b = Combination {coeffs :: [(Int, b)]}
  deriving (Functor)

instance Eq b => Eq (Combination b) where
  c == c' = null (coeffs (c - c'))

-- TODO: obviously make this a hashmap, possibly with special cases
-- for very small combinations? Unless hashmap alreayd does this.

showAddTerm :: Show b => (Int, b) -> String
showAddTerm (0, b) = error "showTerm: 0 coefficient"
showAddTerm (1, b) = show b
showAddTerm (-1, b) = " - " ++ show b
showAddTerm (c, b)
  | c < 0 = " - " ++ show (-c) ++ "·" ++ show b
  | otherwise = " + " ++ show c ++ "·" ++ show b

showSoloTerm :: Show b => (Int, b) -> String
showSoloTerm (0, b) = error "showSoloTerm: 0 coefficient"
showSoloTerm (1, b) = show b
showSoloTerm (-1, b) = "-" ++ show b
showSoloTerm (c, b) = show c ++ "·" ++ show b

instance Show b => Show (Combination b) where
  -- TODO: this reverses order, does anyone care?
  show (Combination []) = "0"
  show (Combination [t]) = showSoloTerm t
  show (Combination (t : cs)) = show cs ++ showAddTerm t

coeffOf :: (Eq b) => Combination b -> b -> Int
coeffOf (Combination l) b = fromMaybe 0 $ fst <$> find (\(c, b') -> b == b') l

merge :: (Foldable t, Eq b, Num a, Eq a) => [(a, b)] -> t (a, b) -> [(a, b)]
merge cs cs' = foldl (flip insert) cs cs'
  where
    insert (0, b) cs' = cs'
    insert (i, b) [] = [(i, b)]
    insert (i, b) ((j, b') : cs')
      | b == b' && i + j == 0 = cs'
      | b == b' = (i + j, b) : cs'
      | otherwise = (j, b') : insert (i, b) cs'

-- Whatever
normalise :: (Eq b, Num a, Eq a) => [(a, b)] -> [(a, b)]
normalise = foldr (\c -> merge [c]) []

(.*) :: Int -> Combination b -> Combination b
0 .* (Combination bs) = Combination []
n .* (Combination bs) = Combination $ fmap (\(c, b) -> (n * c, b)) bs

singleComb :: b -> Combination b
singleComb a = Combination [(1, a)]

instance Constrained.Functor (Constrained.Sub Eq (->)) (->) Combination where
  fmap (Constrained.Sub f) (Combination cs) = Combination $ normalise $ fmap (fmap f) cs

instance Constrained.Functor (Constrained.Sub Eq (->)) (Constrained.Sub Eq (->)) Combination where
  fmap f = Constrained.Sub $ Constrained.fmap f

instance Constrained.Monad (Constrained.Sub Eq (->)) Combination where
  return = Constrained.Sub $ \a -> Combination [(1, a)]
  join = Constrained.Sub $ \(Combination cs) -> foldr (\(n, c1) c2 -> (n .* c1) + c2) 0 cs

instance (Eq b) => Num (Combination b) where
  fromInteger 0 = Combination []
  fromInteger _ = error "Combination: fromInteger"

  (Combination cs) + (Combination cs') = Combination $ merge cs cs'
  negate (Combination cs) = Combination $ fmap (\(n, c) -> (negate n, c)) cs

  (*) = error "Combination: (*)"
  abs = error "Combination: abs"
  signum = error "Combination: signum"
