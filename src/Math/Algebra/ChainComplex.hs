{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Chain complex of free \(ℤ\)-modules
module Math.Algebra.ChainComplex where

import Control.Category.Constrained (id, (.))
import qualified Control.Category.Constrained as Constrained
import Control.Exception (evaluate)
import Data.Coerce
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Prelude hiding (Bounded, id, return, (.))
import System.IO.Unsafe (unsafePerformIO)

import Math.Algebra.AbGroupPres
import Math.Algebra.Combination
import Math.Algebra.Group
import Math.Algebra.SmithNormalForm
import Math.ValueCategory (Arrow, mor)
import Math.ValueCategory.Abelian
import Math.ValueCategory.Additive

class Ord (Basis a) => ChainComplex a where
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
validComb a combination = and $ fmap (\(_, b) -> isBasis a b) (coeffs combination)

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

-- | Memoise a pure function for the lifetime of the returned closure.
--
-- The algorithms in this package build large morphisms out of recursive
-- contractions.  Those contractions describe DAGs, but evaluating them as
-- ordinary functions expands the DAG into a tree.  This helper lets the
-- recursive morphisms retain one result per visited basis element instead.
memoiseOrd :: Ord a => (a -> b) -> a -> b
memoiseOrd f = unsafePerformIO $ do
  cache <- newIORef Map.empty
  pure $ \key -> unsafePerformIO $ do
    current <- readIORef cache
    case Map.lookup key current of
      Just value -> pure value
      Nothing -> do
        value <- evaluate (f key)
        atomicModifyIORef' cache $ \latest ->
          case Map.lookup key latest of
            Just existing -> (latest, existing)
            Nothing -> (Map.insert key value latest, value)
{-# NOINLINE memoiseOrd #-}

-- | Retain the image of each visited source basis element.
memoiseMorphism :: Ord a => UMorphism d a b -> UMorphism d a b
memoiseMorphism (Morphism morphismDegree action) =
  Morphism morphismDegree (memoiseOrd action)

-- | Identical to `onBasis`, but sometimes clearer
underlyingFunction :: UMorphism d a b -> (a -> Combination b)
underlyingFunction = onBasis

instance Constrained.Functor (UMorphism d) (->) Combination where
  fmap m combination = bindCombination combination (m `onBasis`)

onComb :: (Ord a, Ord b) => UMorphism d a b -> Combination a -> Combination b
onComb = Constrained.fmap

morphismZeroOfDeg :: d -> UMorphism d a b
morphismZeroOfDeg d = Morphism d (const zeroCombination)

morphismZero :: Num d => UMorphism d a b
morphismZero = morphismZeroOfDeg 0

fmapBasis :: Num d => (a -> b) -> UMorphism d a b
fmapBasis f = Morphism 0 (singleComb . f)

instance Show d => Show (UMorphism d a b) where
  -- TODO
  show (Morphism d f) = "Morphism of degree " ++ show d

instance Num d => Constrained.Semigroupoid (UMorphism d) where
  type Object (UMorphism d) a = Ord a

  (Morphism d2 f2) . (Morphism d1 f1) =
    Morphism (d1 + d2) (\basis -> bindCombination (f1 basis) f2)

instance Num d => Constrained.Category (UMorphism d) where
  id = Morphism 0 singleComb

instance (Num d, Ord b) => Num (UMorphism d a b) where
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
  type Object ClosedMorphism o = Ord (Basis o)
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
fromChainGrpElt a n (AbGroupPresElt m) =
  fromTerms $ zip (fromIntegral <$> M.toList m) (basis a n)

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

chainDiffs :: FiniteType a => a -> [Arrow AbGroupPres]
chainDiffs a = fmap (chainDiff a) [0 ..]

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
    diffs = chainDiffs a
    pairs = zip (tail diffs) diffs

homologyGroups :: FiniteType a => a -> [HomologyGroup a]
homologyGroups a = fmap (\(n, f,g) -> HomologyGroup n (homology f g) a) pairs
  where
    diffs = chainDiffs a
    pairs = zip3 [0 ..] (tail diffs) diffs

-- | A coordinate of the fundamental cohomology class associated to a
-- cyclic summand of a homology group.  'Nothing' denotes an infinite
-- cyclic summand; 'Just n' denotes coefficients in Z/n.
data FundamentalCocycle a = FundamentalCocycle
  { cocycleOrder :: Maybe Integer,
    cocycleMorphism :: Morphism a ()
  }

-- | Degree in which the cocycle is supported.
cocycleDegree :: FundamentalCocycle a -> Int
cocycleDegree = negate . morphismDegree . cocycleMorphism

-- | Fundamental cocycles for the cyclic invariant factors of H_n(a).
fundamentalCocycles :: FiniteType a => a -> Int -> Either String [FundamentalCocycle a]
fundamentalCocycles a n =
  fundamentalCocyclesWithDiffs a n (chainDiff a n) (chainDiff a (n + 1))

fundamentalCocyclesWithDiffs ::
  FiniteType a =>
  a ->
  Int ->
  Arrow AbGroupPres ->
  Arrow AbGroupPres ->
  Either String [FundamentalCocycle a]
fundamentalCocyclesWithDiffs a n outgoing incoming
  | isExact incoming outgoing = Right []
  | otherwise = do
      boundaryCoordinates <-
        maybe
          (Left "boundaries are not contained in cycles")
          Right
          (solveMatrix cycles boundaries)
      let Triple leftChange _ smith _ _ = smithNormalForm boundaryCoordinates
          diagonal = take cycleRank $ V.toList (M.getDiag smith) ++ repeat 0
          nontrivialRows = filter ((/= 1) . snd) $ zip [1 ..] diagonal
      traverse (makeCocycle leftChange) nontrivialRows
  where
    cycles = matrixKernel (fullMorphism (mor outgoing))
    boundaries = fullMorphism (mor incoming)

    cycleRank = M.ncols cycles

    makeCocycle leftChange (i, order) = do
      let cycleValues = M.fromList cycleRank 1 (V.toList (M.getRow i leftChange))
      functional <-
        maybe
          (Left "fundamental cocycle does not extend to the full chain group")
          Right
          (solveMatrix (M.transpose cycles) cycleValues)
      let functionalValues = zip (basis a n) (M.toList functional)
          act b
            | degree a b /= n = 0
            | otherwise = case lookup b functionalValues of
                Just value -> fromInteger value .* singleComb ()
                Nothing -> error "fundamentalCocycles: invalid basis element"
      Right $ FundamentalCocycle (if order == 0 then Nothing else Just order) (Morphism (negate n) act)

neghomologies :: FiniteType a => a -> [AbGroupPres]
neghomologies a = fmap (uncurry homology) pairs
  where
    diffs = fmap (chainDiff a . negate) [-1 ..]
    pairs = zip diffs (tail diffs)
