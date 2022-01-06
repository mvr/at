{-# LANGUAGE UndecidableInstances #-}

-- | Chain complex of free Z-modules
module Math.Algebra.ChainComplex where

import Control.Category.Constrained
import Data.List (find)
import qualified Data.Matrix as M
import Data.Maybe (fromJust)
import Math.Algebra.AbGroup
import Math.ValueCategory (Arrow)
import Math.ValueCategory.Abelian
import Math.ValueCategory.Additive
import Prelude hiding (id, (.))

class ChainComplex a where
  type Basis a

  isBasis :: a -> Basis a -> Bool
  isBasis _ _ = True

  degree :: a -> Basis a -> Int
  diff :: a -> Morphism a a

instance ChainComplex () where
  type Basis () = ()
  degree _ _ = 0
  diff _ = 0

class ChainComplex a => LevelwiseFinite a where
  dim :: a -> Int -> Int
  -- * `all isSimplex (basis n)`
  basis :: a -> Int -> [Basis a]

instance LevelwiseFinite () where
  dim _ 0 = 1
  dim _ _ = 0
  basis _ 0 = [()]
  basis _ _ = []

newtype Combination b = Combination {coeffs :: [(Int, b)]}
  deriving (Show, Functor)

(.*) :: Int -> Combination b -> Combination b
n .* (Combination bs) = Combination $ fmap (\(c, b) -> (n * c, b)) bs

flattenComb :: (Eq b) => Combination (Combination b) -> Combination b
flattenComb = undefined

instance (Eq b) => Num (Combination b) where
  fromInteger 0 = Combination []
  fromInteger _ = undefined

  (Combination cs) + (Combination cs') = Combination $ merge cs cs'
    where
      merge cs cs' = foldl (flip insert) cs cs'
      insert (i, b) [] = [(i, b)]
      insert (i, b) ((j, b') : cs')
        | b == b' = (i + j, b) : cs'
        | otherwise = (j, b') : insert (i, b) cs'

coeffOf :: (Eq b) => Combination b -> b -> Int
coeffOf (Combination l) b = fst $ fromJust $ find (\(c, b') -> b == b') l

-- NOTE: I don't think we ever use a variable morphism degree, so the
-- degree could be lifted to the type level. Then again I think
-- type-level Ints are rough compared to Nats.
data UMorphism a b = Morphism
  { morphismDegree :: Int,
    onBasis :: a -> Combination b
  }

type Morphism a b = UMorphism (Basis a) (Basis b)

onComb :: (Eq b) => UMorphism a b -> Combination a -> Combination b
onComb m c = flattenComb $ fmap (m `onBasis`) c

morphismZeroOfDeg :: Int -> UMorphism a b
morphismZeroOfDeg d = Morphism d (const (Combination []))

morphismZero :: UMorphism a b
morphismZero = morphismZeroOfDeg 0

instance Category UMorphism where
  type Object UMorphism a = Eq a

  id = Morphism 0 (\x -> Combination [(1, x)])
  (Morphism d2 f2) . (Morphism d1 f1) = Morphism (d1 + d2) (flattenComb . fmap f2 . f1)

instance Eq b => Num (UMorphism a b) where
  fromInteger 0 = morphismZero
  fromInteger _ = undefined

  (Morphism d1 f1) + (Morphism _ f2) = Morphism d1 (\x -> f1 x + f2 x)

data ClosedMorphism a b = ClosedMorphism a (Morphism a b) b
-- For convenience, not really legal.
instance Category ClosedMorphism where
  type Object ClosedMorphism o = Eq (Basis o)
  (ClosedMorphism _ n c) . (ClosedMorphism a m _) = ClosedMorphism a (n . m) c

chainGroup :: (LevelwiseFinite a) => a -> Int -> AbGroup
chainGroup a n | n < 0 = zero
chainGroup a n
  | d == 0 = zero -- Annoying that I have to do this
  | otherwise = freeAbGroup (fromIntegral d)
  where
    d = dim a n

chainDiff :: (Eq (Basis a), LevelwiseFinite a) => a -> Int -> Arrow AbGroup
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

homologies :: (Eq (Basis a), LevelwiseFinite a) => a -> [AbGroup]
homologies a = fmap (uncurry homology) pairs
  where
    diffs = fmap (chainDiff a) [0 ..]
    pairs = zip (tail diffs) diffs
