-- | The standard \(n\)-simplex

module Math.Topology.SSet.NSimplex where

import Math.Topology.SSet
import Math.Topology.SSet.Effective

newtype NSimplex = NSimplex { simplexDimension :: Int }

instance Show NSimplex where
  show (NSimplex n) = "Δ^" ++ show n

newtype NSimplexSimplex = NSimplexSimplex [Int]
  deriving (Eq, Ord, Show)

isOrdered :: (Ord a) => [a] -> Bool
isOrdered [] = True
isOrdered [x] = True
isOrdered (x:y:xs) = (x <= y) && isOrdered (y:xs)

deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt 0 (x:xs) = xs
deleteAt i (x:xs) = x : deleteAt (i - 1) xs

instance SSet NSimplex where
  type GeomSimplex NSimplex = NSimplexSimplex
  geomSimplexDim (NSimplex d) (NSimplexSimplex vs) = length vs - 1
  isGeomSimplex (NSimplex d) (NSimplexSimplex vs) = length vs <= d+1 && isOrdered vs
  geomFace (NSimplex d) (NSimplexSimplex vs) i = NonDegen (NSimplexSimplex (deleteAt i vs))

-- TODO: could be more efficient
choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose i [] = []
choose i (x:xs) = fmap (x:) (choose (i-1) xs) ++ choose i xs

instance FiniteType NSimplex where
  geomBasis (NSimplex d) i = NSimplexSimplex <$> choose (i + 1) [0..d]

instance Pointed NSimplex where
  basepoint _ = NSimplexSimplex [0]

instance Effective NSimplex
