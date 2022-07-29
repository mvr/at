{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Chain complex of free \(ℤ\)-modules
module Math.Algebra.ChainComplex where

import Control.Category.Constrained (id, incl, join, (.))
import qualified Control.Category.Constrained as Constrained
import Data.Coerce
import qualified Data.Matrix as M
import Prelude hiding (Bounded, id, return, (.))

import Math.Algebra.AbGroupPres
import Math.Algebra.Combination
import Math.Algebra.Group
import Math.ValueCategory (Arrow)
import Math.ValueCategory.Abelian
import Math.ValueCategory.Additive

class (Eq (Basis a)) => ChainComplex a where
  type Basis a = s | s -> a

  isBasis :: a -> Basis a -> Bool
  isBasis _ _ = True

  degree :: a -> Basis a -> Int
  diff :: a -> Morphism a a

type Chain a = Combination (Basis a)

instance ChainComplex () where
  type Basis () = ()
  degree _ _ = 0
  diff _ = 0

class ChainComplex a => FiniteType a where
  dim :: a -> Int -> Int
  dim a i = length (basis a i)
  -- * `all isSimplex (basis n)`
  basis :: a -> Int -> [Basis a]

instance FiniteType () where
  dim _ 0 = 1
  dim _ _ = 0
  basis _ 0 = [()]
  basis _ _ = []

class ChainComplex a => Bounded a where
  -- | Dimensions with non-zero chains
  amplitude :: a -> [Int]

instance Bounded () where
  amplitude _ = [0]

validComb :: ChainComplex a => a -> Chain a -> Bool
validComb a (Combination bs) = and $ fmap (\(_, b) -> isBasis a b) bs

-- well, not really
kozulRule :: Num b => Int -> b -> b
kozulRule n c = if even n then c else negate c

-- NOTE: I don't think we ever use a variable morphism degree, so the
-- degree could be lifted to the type level. Then again I think
-- type-level Ints are rough compared to Nats.
data UMorphism d a b = Morphism
  { morphismDegree :: d,
    onBasis :: a -> Combination b
  }

type Morphism a b = UMorphism Int (Basis a) (Basis b)

instance Constrained.Functor (UMorphism d) (->) Combination where
  fmap m c = incl (join @(Constrained.Sub Eq (->))) $ fmap (m `onBasis`) c

onComb :: (Eq a, Eq b) => UMorphism d a b -> Combination a -> Combination b
onComb = Constrained.fmap

morphismZeroOfDeg :: d -> UMorphism d a b
morphismZeroOfDeg d = Morphism d (const (Combination []))

morphismZero :: Num d => UMorphism d a b
morphismZero = morphismZeroOfDeg 0

fmapBasis :: Num d => (a -> b) -> UMorphism d a b
fmapBasis f = Morphism 0 (singleComb . f)

instance Show d => Show (UMorphism d a b) where
  -- TODO
  show (Morphism d f) = "Morphism of degree " ++ show d

instance Num d => Constrained.Semigroupoid (UMorphism d) where
  type Object (UMorphism d) a = Eq a

  (Morphism d2 f2) . (Morphism d1 f1) = Morphism (d1 + d2) (incl (join @(Constrained.Sub Eq (->))) . fmap f2 . f1)

instance Num d => Constrained.Category (UMorphism d) where
  id = Morphism 0 (\x -> Combination [(1, x)])

instance (Num d, Eq b) => Num (UMorphism d a b) where
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

data ChainGroup a = ChainGroup Int a
newtype ChainGroupElt a = ChainGroupElt (Combination a)

instance (ChainComplex a) => Group (ChainGroup a) where
  type Element (ChainGroup a) = ChainGroupElt (Basis a)
  prod _ = coerce ((+) :: Chain a -> Chain a -> Chain a)
  inv _ = coerce (negate :: Chain a -> Chain a)
  unit _ = coerce (0 :: Chain a)

instance Constrained.Functor (UMorphism d) (->) ChainGroupElt where
  fmap m (ChainGroupElt as) = ChainGroupElt $ m `onComb` as

toChainGrpElt :: (FiniteType a) => a -> Int -> Chain a -> AbGroupPresElt
toChainGrpElt a n cs = AbGroupPresElt $ M.fromList 1 (length r) (fmap (fromIntegral . coeffOf cs) r)
  where
    r = basis a n

fromChainGrpElt :: (FiniteType a) => a -> Int -> AbGroupPresElt -> Chain a
fromChainGrpElt a n (AbGroupPresElt m) = Combination $ filter (\(c, _) -> c /= 0) $ zip (fromIntegral <$> M.toList m) (basis a n)

chainGroup :: FiniteType a => a -> Int -> AbGroupPres
-- chainGroup a n | n < 0 = zero
chainGroup a n = freeAbGroup (fromIntegral (dim a n))

chainDiff :: FiniteType a => a -> Int -> Arrow AbGroupPres
-- chainDiff a n | n < 0 = zeroArrow zero zero
-- chainDiff a 0 = toZero (chainGroup a 0)
chainDiff a n
  | rows == 0 && cols == 0 = zeroArrow zero zero
  | rows == 0 = toZero (chainGroup a n)
  | cols == 0 = fromZero (chainGroup a (n - 1))
  | otherwise =
    morphismFromFullMatrix
      (chainGroup a n)
      (chainGroup a (n - 1))
      (M.matrix rows cols findCoef)
  where
    rows = dim a (n - 1)
    cols = dim a n
    codbasis = basis a (n - 1)
    dombasis = basis a n
    images = fmap (onBasis (diff a)) dombasis
    findCoef (i, j) = fromIntegral $ coeffOf (images !! (j - 1)) (codbasis !! (i - 1))

-- Only compute the group presentation once
data HomologyGroup a = HomologyGroup Int AbGroupPres a

instance Show a => Show (HomologyGroup a) where
  show (HomologyGroup n p a) = "H^" ++ show n ++ "(" ++ show a ++ ")=" ++ show p

newtype UHomologyClass a = HomologyClass (Combination a)
type HomologyClass a = UHomologyClass (Basis a)

instance Show a => (Show (UHomologyClass a)) where
  show (HomologyClass cs) = show cs

classRepresentative :: HomologyClass a -> Chain a
classRepresentative (HomologyClass cs) = cs

instance (FiniteType a) => Group (HomologyGroup a) where
  type Element (HomologyGroup a) = HomologyClass a
  prod (HomologyGroup n p a) (HomologyClass x) (HomologyClass y) = HomologyClass $ fromChainGrpElt a n $ prod p (toChainGrpElt a n x) (toChainGrpElt a n y)
  inv (HomologyGroup n p a) (HomologyClass x) = HomologyClass $ fromChainGrpElt a n $ inv p (toChainGrpElt a n x)
  unit (HomologyGroup n p a) = HomologyClass $ fromChainGrpElt a n (unit p)

instance Constrained.Functor (UMorphism d) (->) UHomologyClass where
  fmap m (HomologyClass as) = HomologyClass $ m `onComb` as

homologyGenerators :: FiniteType a => HomologyGroup a -> [HomologyClass a]
homologyGenerators (HomologyGroup n p a) = fmap HomologyClass chains
  where chains = fmap (fromChainGrpElt a n . AbGroupPresElt . (fromReduced p *) . eltVector) (indGenerators p)

homologies :: FiniteType a => a -> [AbGroupPres]
homologies a = fmap (uncurry homology) pairs
  where
    diffs = fmap (chainDiff a) [0 ..]
    pairs = zip (tail diffs) diffs

homologyGroups :: FiniteType a => a -> [HomologyGroup a]
homologyGroups a = fmap (\(n, f,g) -> HomologyGroup n (homology f g) a) pairs
  where
    diffs = fmap (chainDiff a) [0 ..]
    pairs = zip3 [0 ..] (tail diffs) diffs

neghomologies :: FiniteType a => a -> [AbGroupPres]
neghomologies a = fmap (uncurry homology) pairs
  where
    diffs = fmap (chainDiff a . negate) [-1 ..]
    pairs = zip diffs (tail diffs)
