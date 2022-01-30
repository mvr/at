{-# LANGUAGE UndecidableInstances #-}

-- | Chain complex of free Z-modules
module Math.Algebra.ChainComplex where

import Control.Category.Constrained (id, join, (.), return)
import qualified Control.Category.Constrained as Constrained
import Data.List (find)
import qualified Data.Matrix as M
import Data.Maybe (fromMaybe)
import Math.Algebra.AbGroupPres
import Math.ValueCategory (Arrow)
import Math.ValueCategory.Abelian
import Math.ValueCategory.Additive
import Prelude hiding (id, (.), return)

class (Eq (Basis a)) => ChainComplex a where
  type Basis a = s | s -> a

  isBasis :: a -> Basis a -> Bool
  isBasis _ _ = True

  degree :: a -> Basis a -> Int
  diff :: a -> Morphism a a

instance ChainComplex () where
  type Basis () = ()
  degree _ _ = 0
  diff _ = 0

class ChainComplex a => FiniteType a where
  dim :: a -> Int -> Int
  -- * `all isSimplex (basis n)`
  basis :: a -> Int -> [Basis a]

instance FiniteType () where
  dim _ 0 = 1
  dim _ _ = 0
  basis _ 0 = [()]
  basis _ _ = []

-- | Z-linear combinations
newtype Combination b = Combination {coeffs :: [(Int, b)]}
  deriving (Functor)

instance Eq b => Eq (Combination b) where
  c == c' = null (coeffs (c - c'))

-- TODO: obviously make this a hashmap, possibly with special cases
-- for very small combinations? Unless hashmap alreayd does this.

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

kozulRule :: Eq b => Int -> Combination b -> Combination b
kozulRule n c = if even n then c else negate c

singleComb :: b -> Combination b
singleComb a = Combination [(1, a)]

instance Constrained.Functor (->) (->) Combination where
  type CodObj Combination a = Eq a
  fmap f (Combination cs) = Combination $ normalise $ fmap (fmap f) cs

instance Constrained.Monad (->) Combination where
  return a = Combination [(1, a)]
  join (Combination cs) = foldr (\(n, c1) c2 -> (n .* c1) + c2) 0 cs

instance (Eq b) => Num (Combination b) where
  fromInteger 0 = Combination []
  fromInteger _ = error "Combination: fromInteger"

  (Combination cs) + (Combination cs') = Combination $ merge cs cs'
  negate (Combination cs) = Combination $ fmap (\(n, c) -> (negate n, c)) cs

  (*) = error "Combination: (*)"
  abs = error "Combination: abs"
  signum = error "Combination: signum"

-- NOTE: I don't think we ever use a variable morphism degree, so the
-- degree could be lifted to the type level. Then again I think
-- type-level Ints are rough compared to Nats.
data UMorphism a b = Morphism
  { morphismDegree :: Int,
    onBasis :: a -> Combination b
  }

type Morphism a b = UMorphism (Basis a) (Basis b)

onComb :: (Eq b) => UMorphism a b -> Combination a -> Combination b
onComb m c = join $ fmap (m `onBasis`) c

morphismZeroOfDeg :: Int -> UMorphism a b
morphismZeroOfDeg d = Morphism d (const (Combination []))

morphismZero :: UMorphism a b
morphismZero = morphismZeroOfDeg 0

fmapBasis :: (a -> b) -> UMorphism a b
fmapBasis f = Morphism 0 (singleComb . f)

instance Show (UMorphism a b) where
  -- TODO
  show (Morphism d f) = "Morphism " ++ show d

instance Constrained.Semigroupoid UMorphism where
  type Object UMorphism a = Eq a

  (Morphism d2 f2) . (Morphism d1 f1) = Morphism (d1 + d2) (join . fmap f2 . f1)

instance Constrained.Category UMorphism where
  id = Morphism 0 (\x -> Combination [(1, x)])

instance Eq b => Num (UMorphism a b) where
  fromInteger 0 = morphismZero
  fromInteger _ = error "Morphism: fromInteger"

  (Morphism d1 f1) + (Morphism _ f2) = Morphism d1 (\x -> f1 x + f2 x)
  negate (Morphism d f) = Morphism d (negate . f)

  (*) = error "Morphism: (*)"
  abs = error "Morphism: abs"
  signum = error "Morphism: signum"

-- | A `ClosedMorphism` is a morphism that includes the data of its
-- endpoints. (Closed as in a closed interval.) We cannot define id,
-- because we would need the data of `a`.
data ClosedMorphism a b = ClosedMorphism a (Morphism a b) b

instance Constrained.Semigroupoid ClosedMorphism where
  type Object ClosedMorphism o = Eq (Basis o)
  (ClosedMorphism _ n c) . (ClosedMorphism a m _) = ClosedMorphism a (n . m) c

chainGroup :: FiniteType a => a -> Int -> AbGroupPres
chainGroup a n | n < 0 = zero
chainGroup a n = freeAbGroup (fromIntegral (dim a n))

chainDiff :: FiniteType a => a -> Int -> Arrow AbGroupPres
chainDiff a n | n < 0 = zeroArrow zero zero
chainDiff a 0 = toZero (chainGroup a 0)
chainDiff a n
  | rows == 0 && cols == 0 = zeroArrow zero zero
  | rows == 0 = toZero (chainGroup a n)
  | cols == 0 = fromZero (chainGroup a (n -1))
  | otherwise =
    morphismFromFullMatrix
      (chainGroup a n)
      (chainGroup a (n - 1))
      (M.matrix rows cols findCoef)
  where
    rows = dim a (n -1)
    cols = dim a n
    codbasis = basis a (n - 1)
    dombasis = basis a n
    images = fmap (onBasis (diff a)) dombasis
    findCoef (i, j) = fromIntegral $ coeffOf (images !! (j - 1)) (codbasis !! (i - 1))

homologies :: FiniteType a => a -> [AbGroupPres]
homologies a = fmap (uncurry homology) pairs
  where
    diffs = fmap (chainDiff a) [0 ..]
    pairs = zip (tail diffs) diffs
