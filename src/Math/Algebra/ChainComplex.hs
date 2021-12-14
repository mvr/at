-- | Chain complex of free Z-modules

module Math.Algebra.ChainComplex where

import qualified Data.Matrix as M

import Math.Algebra.AbGroup
import Math.ValueCategory.Abelian
import Data.Maybe (fromJust)
import Data.List (find)
import Math.ValueCategory ( Arrow )
import Math.ValueCategory.Additive

import Debug.Trace

class ChainComplex a where
  type Basis a
  diff :: a -> ChainMorphism a a

class ChainComplex a => LevelwiseFiniteCC a where
  dim :: a -> Int -> Int
  -- * `all isSimplex (basis n)`
  basis :: a -> Int -> [Basis a]

newtype Combination b = Combination { coeffs :: [(Int, b)] }
  deriving (Show)

instance (Eq b) => Num (Combination b) where
  fromInteger 0 = Combination []
  (Combination cs) + (Combination cs') = Combination $ merge cs cs'
    where
      merge cs cs' = foldl  (flip insert) cs cs'
      insert (i, b) [] = [(i, b)]
      insert (i, b) ((j, b'):cs') | b == b' = (i + j, b) : cs'
                                   | otherwise = (j, b') : insert (i, b) cs'

coeffOf :: (Eq b) => Combination b -> b -> Int
coeffOf (Combination l) b = fst $ fromJust $ find (\(c,b') -> b == b') l

data ChainMorphism a b = ChainMorphism
  { degree :: Int,
    onBasis :: Basis a -> Combination (Basis b)
  }

chainGroup :: (LevelwiseFiniteCC a) => a -> Int -> AbGroup
chainGroup a n | d == 0 = zero  -- Annoying that I have to do this
               | otherwise = freeAbGroup (fromIntegral $ dim a n)
  where d = dim a n

chainDiff :: (Eq (Basis a), LevelwiseFiniteCC a) => a -> Int -> Arrow AbGroup
chainDiff a n
  | rows == 0 && cols == 0 = zeroArrow zero zero
  | rows == 0 = toZero (chainGroup a (n+1))
  | cols == 0 = fromZero (chainGroup a n)
  | otherwise = morphismFromFullMatrix
                  (chainGroup a (n+1))
                  (chainGroup a n)
                  (M.matrix cols rows findCoef)
  where rows = dim a n
        cols = dim a (n+1)
        dombasis = basis a (n+1)
        codbasis = basis a n
        images = fmap (onBasis (diff a)) dombasis
        findCoef (i, j) = fromIntegral $ coeffOf (images !! (i - 1)) (codbasis !! (j-1))

homologies :: (Eq (Basis a), LevelwiseFiniteCC a) => a -> [AbGroup]
homologies a = fmap (uncurry homology) pairs
  where diffs = fmap (chainDiff a) [0..]
        pairs = zip diffs (toZero (chainGroup a 0):diffs)
