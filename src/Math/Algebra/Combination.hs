module Math.Algebra.Combination where

import Control.Category.Constrained (join, return)
import qualified Control.Category.Constrained as Constrained
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Prelude hiding (id, return, (.))

-- | Z-linear combinations
newtype Combination b = Combination {coeffs :: [(Int, b)]}
  deriving (Functor)

instance Applicative Combination where
  pure b = Combination [(1, b)]
  (Combination fs) <*> (Combination as) = Combination $ do
    (fc, f) <- fs
    (ac, a) <- as
    return (fc * ac, f a)

instance Ord b => Eq (Combination b) where
  Combination cs == Combination cs' = normalise cs == normalise cs'

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

coeffOf :: Ord b => Combination b -> b -> Int
coeffOf (Combination terms) b = Map.findWithDefault 0 b (termMap terms)

termMap :: (Ord b, Num a) => [(a, b)] -> Map.Map b a
termMap terms = Map.fromListWith (+) (fmap swap terms)

merge :: (Foldable t, Ord b, Num a, Eq a) => [(a, b)] -> t (a, b) -> [(a, b)]
merge terms terms' = normalise (terms ++ toList terms')

normalise :: (Ord b, Num a, Eq a) => [(a, b)] -> [(a, b)]
normalise terms = fmap swap (Map.toAscList (Map.filter (/= 0) (termMap terms)))

(.*) :: Int -> Combination b -> Combination b
0 .* (Combination bs) = Combination []
n .* (Combination bs) = Combination $ fmap (\(c, b) -> (n * c, b)) bs

singleComb :: b -> Combination b
singleComb a = Combination [(1, a)]

-- TODO: generalise via Constrained.Traversable
-- traverseComb :: (Ord b) => (a -> Combination b) -> [a] -> Combination [b]
-- traverseComb f [] = 0
-- traverseComb f (a:as) = liftA2 (:)

instance Constrained.Functor (Constrained.Sub Ord (->)) (->) Combination where
  fmap (Constrained.Sub f) (Combination cs) = Combination $ normalise $ fmap (fmap f) cs

instance Constrained.Functor (Constrained.Sub Ord (->)) (Constrained.Sub Ord (->)) Combination where
  fmap f = Constrained.Sub $ Constrained.fmap f

instance Constrained.Monad (Constrained.Sub Ord (->)) Combination where
  return = Constrained.Sub $ \a -> Combination [(1, a)]
  join = Constrained.Sub $ \(Combination cs) -> foldr (\(n, c1) c2 -> (n .* c1) + c2) 0 cs

instance Ord b => Num (Combination b) where
  fromInteger 0 = Combination []
  fromInteger _ = error "Combination: fromInteger"

  (Combination cs) + (Combination cs') = Combination $ merge cs cs'
  negate (Combination cs) = Combination $ fmap (\(n, c) -> (negate n, c)) cs

  (*) = error "Combination: (*)"
  abs = error "Combination: abs"
  signum = error "Combination: signum"
