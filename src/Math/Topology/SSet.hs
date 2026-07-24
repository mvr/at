{-# LANGUAGE PatternSynonyms #-}

module Math.Topology.SSet where

import qualified Control.Category.Constrained as Constrained
import Control.Monad (ap)
import Data.Bits
import Prelude hiding (Bounded)

-- A formal degeneracy is stored as a bit mask, with pattern synonyms
-- retaining the recursive interface used by the simplicial identities.
data FormalDegen a = FormalDegen {-# UNPACK #-} !Word a
  deriving (Eq, Ord, Functor)
  deriving (Constrained.Functor (->) (->)) via (Constrained.Wrapped FormalDegen)

pattern NonDegen :: a -> FormalDegen a
pattern NonDegen a = FormalDegen 0 a

pattern Degen :: Int -> FormalDegen a -> FormalDegen a
pattern Degen i s <- (splitDegen -> Just (i, s))
  where
    Degen i (FormalDegen mask a)
      | i < 0 || i >= finiteBitSize mask = error "Degen: invalid index"
      | otherwise = FormalDegen (setBit mask i) a

{-# COMPLETE NonDegen, Degen #-}

highestSetBit :: Word -> Int
highestSetBit mask = finiteBitSize mask - countLeadingZeros mask - 1

splitDegen :: FormalDegen a -> Maybe (Int, FormalDegen a)
splitDegen (FormalDegen 0 _) = Nothing
splitDegen (FormalDegen mask a) =
  let i = highestSetBit mask
   in Just (i, FormalDegen (clearBit mask i) a)

maskIndices :: Word -> [Int]
maskIndices 0 = []
maskIndices mask =
  let i = highestSetBit mask
   in i : maskIndices (clearBit mask i)

instance Show a => Show (FormalDegen a) where
  show (FormalDegen mask a) =
    concatMap (\i -> "s_" ++ show i ++ " ") (maskIndices mask) ++ show a

instance Applicative FormalDegen where
  pure = NonDegen
  (<*>) = ap

instance Monad FormalDegen where
  FormalDegen mask s >>= f = applyMask mask (f s)
    where
      applyMask 0 result = result
      applyMask remaining result =
        let i = countTrailingZeros remaining
         in applyMask (clearBit remaining i) (degen result i)

isDegen :: FormalDegen a -> Bool
isDegen (FormalDegen mask _) = mask /= 0

underlyingGeom :: FormalDegen a -> a
underlyingGeom (FormalDegen _ s) = s

degen :: FormalDegen a -> Int -> FormalDegen a
degen (FormalDegen mask a) i = FormalDegen (insertDegenBit mask i) a

insertDegenBit :: Word -> Int -> Word
insertDegenBit mask i
  | i < 0 || i >= finiteBitSize mask = error "degen: invalid index"
  | shiftedHigher `shiftR` 1 /= higher = error "degen: degeneracy mask overflow"
  | otherwise = lower .|. bit i .|. shiftedHigher
  where
    lower = mask .&. (bit i - 1)
    higher = mask `xor` lower
    shiftedHigher = higher `shiftL` 1

applyDegenMask :: Word -> FormalDegen a -> FormalDegen a
applyDegenMask operations (FormalDegen mask a) = FormalDegen (go operations mask) a
  where
    go 0 result = result
    go remaining result =
      let i = countTrailingZeros remaining
       in go (clearBit remaining i) (insertDegenBit result i)

deleteDegenBit :: Word -> Int -> Word
deleteDegenBit mask i = lower .|. shiftedHigher
  where
    lowerMask = bit i - 1
    lower = mask .&. lowerMask
    shiftedHigher = (mask `shiftR` 1) .&. complement lowerMask

-- | Delete the indicated positions from a degeneracy mask. Deleting from
-- highest to lowest keeps the remaining operation indices valid.
removeDegenMask :: Word -> Word -> Word
removeDegenMask operations = go operations
  where
    go 0 mask = mask
    go remaining mask =
      let i = highestSetBit remaining
       in go (clearBit remaining i) (deleteDegenBit mask i)

degenList :: FormalDegen a -> [Int]
degenList (FormalDegen mask _) = maskIndices mask

degenCount :: FormalDegen a -> Int
degenCount (FormalDegen mask _) = popCount mask

-- In this representation, we just need to check that the index is
-- somewhere in the list. (Not necessarily the first thing)
isImageOfDegen :: FormalDegen a -> Int -> Bool
isImageOfDegen (FormalDegen mask _) i = i >= 0 && testBit mask i

constantAt :: a -> Int -> FormalDegen a
constantAt a n
  | n < 0 || n > finiteBitSize (0 :: Word) = error "constantAt: invalid dimension"
  | otherwise = FormalDegen (bit n - 1) a

-- The following are dangerous and only make sense in certain situations.
downshiftN :: Int -> FormalDegen a -> FormalDegen a
downshiftN n (FormalDegen mask s)
  | n < 0 || shifted `shiftR` n /= mask = error "downshiftN: invalid shift"
  | otherwise = FormalDegen shifted s
  where
    shifted = mask `shiftL` n

downshift :: FormalDegen a -> FormalDegen a
downshift = downshiftN 1

upshift :: FormalDegen a -> FormalDegen a
upshift (FormalDegen mask s) = FormalDegen (mask `shiftR` 1) s

type Simplex a = FormalDegen (GeomSimplex a)

class Ord (GeomSimplex a) => SSet a where
  -- NOTE: Maybe this shouldn't be an associated type, instead just
  -- another parameter to the typeclass

  -- NOTE: Or we could even reverse things, so that GeomSimplex is the
  -- class and SSet is the associated type.
  type GeomSimplex a = s | s -> a

  -- In a language with dependent types, this could be folded into the
  -- GeomSimplex type.
  isGeomSimplex :: a -> GeomSimplex a -> Bool
  isGeomSimplex _ _ = True

  geomSimplexDim :: a -> GeomSimplex a -> Int

  -- geomSimplexDim a s = length (geomFaces a s)
  geomFace :: a -> GeomSimplex a -> Int -> Simplex a

  geomFaces :: a -> GeomSimplex a -> [Simplex a]
  geomFaces a s =
    let d = geomSimplexDim a s
     in if d == 0 then [] else fmap (geomFace a s) [0 .. d]

  -- | Nondegenerate faces paired with their face indices. Instances may
  -- avoid constructing faces that are known to be degenerate.
  geomNonDegenFaces :: a -> GeomSimplex a -> [(Int, GeomSimplex a)]
  geomNonDegenFaces a s =
    [(i, t) | (i, NonDegen t) <- zip [0 ..] (geomFaces a s)]

isSimplex :: SSet a => a -> Simplex a -> Bool
isSimplex a (FormalDegen mask s) =
  isGeomSimplex a s
    && (mask == 0 || highestSetBit mask <= geomSimplexDim a s + popCount mask - 1)

simplexDim :: SSet a => a -> Simplex a -> Int
simplexDim a (FormalDegen mask s) = geomSimplexDim a s + popCount mask

face :: SSet a => a -> Simplex a -> Int -> Simplex a
face a (FormalDegen mask s) i
  -- d_i cancels s_i or s_(i-1); otherwise it passes through the
  -- degeneracy word, whose removed position shifts all higher indices.
  | testBit mask i = FormalDegen (deleteDegenBit mask i) s
  | i > 0 && testBit mask (i - 1) = FormalDegen (deleteDegenBit mask (i - 1)) s
  | otherwise =
      applyDegenMask
        (deleteDegenBit mask i)
        (geomFace a s (i - popCount (mask .&. (bit i - 1))))

-- | An injective map @[m] -> [n]@ in the simplex category, represented
-- by the vertices of @[n]@ in its image.
data FaceOperator = FaceOperator Int [Int]
  deriving (Eq, Ord, Show)

identityFaceOperator :: Int -> FaceOperator
identityFaceOperator dimension
  | dimension < 0 = error "identityFaceOperator: negative dimension"
  | otherwise = FaceOperator dimension [0 .. dimension]

-- | Precompose a face operator with the indicated face map.
faceOperatorFace :: FaceOperator -> Int -> FaceOperator
faceOperatorFace (FaceOperator sourceDimension vertices) i
  | i < 0 = error "faceOperatorFace: invalid face index"
  | otherwise = case splitAt i vertices of
      (before, _ : after) -> FaceOperator sourceDimension (before ++ after)
      _ -> error "faceOperatorFace: invalid face index"

applyFaceOperator :: SSet a => a -> FaceOperator -> Simplex a -> Simplex a
applyFaceOperator a (FaceOperator sourceDimension vertices) simplex
  | simplexDim a simplex /= sourceDimension =
      error "applyFaceOperator: source dimension mismatch"
  | otherwise = foldl (face a) simplex omittedVertices
  where
    omittedVertices =
      [i | i <- [sourceDimension, sourceDimension - 1 .. 0], i `notElem` vertices]

hasFace :: SSet a => a -> GeomSimplex a -> GeomSimplex a -> Bool
hasFace a t s = NonDegen s `elem` geomFaces a t

frontFace :: SSet a => a -> Simplex a -> Simplex a
frontFace a s = face a s 0

backFace :: SSet a => a -> Simplex a -> Simplex a
backFace a s = face a s (simplexDim a s)

class SSet a => FiniteType a where
  -- * `all isSimplex (geomBasis n)`
  geomBasis :: a -> Int -> [GeomSimplex a]

someSimplices :: (SSet a) => a -> Int -> (Int -> [GeomSimplex a]) -> [Simplex a]
someSimplices a n f | n < 0 = []
someSimplices a n f = fmap NonDegen (f n) ++ (degensOf =<< someSimplices a (n - 1) f)
  where
    degensOf s@(NonDegen g) = fmap (\i -> Degen i s) [0 .. simplexDim a s]
    degensOf s@(Degen j _)  = fmap (\i -> Degen i s) [(j + 1) .. simplexDim a s]

allSimplices :: (FiniteType a) => a -> Int -> [Simplex a]
allSimplices a n = someSimplices a n (geomBasis a)

class SSet a => Bounded a where
  amplitude :: a -> [Int]

class SSet a => Pointed a where
  basepoint :: a -> GeomSimplex a

basepointSimplex :: (Pointed a) => a -> Simplex a
basepointSimplex a = NonDegen (basepoint a)

-- TODO: move Pointed to its own file to import Morphism
-- basepointMor :: a -> Morphism () a

-- | SSet with a unique 0-simplex.
class Pointed a => ZeroReduced a

-- | SSets with no non-degenerate 1-simplices.
class ZeroReduced a => OneReduced a

-- | Simplicial morphisms
newtype UMorphism a b = Morphism {onGeomSimplex :: a -> FormalDegen b}

type Morphism a b = UMorphism (GeomSimplex a) (GeomSimplex b)

onSimplex :: UMorphism a b -> FormalDegen a -> FormalDegen b
onSimplex (Morphism f) (FormalDegen mask s) = applyDegenMask mask (f s)

instance Constrained.Semigroupoid UMorphism where
  f2 . (Morphism f1) = Morphism $ \s -> f2 `onSimplex` f1 s

instance Constrained.Category UMorphism where
  id = Morphism $ \s -> NonDegen s

instance Constrained.Functor UMorphism (->) FormalDegen where
  fmap = onSimplex

-- Reid Barton:
-- https://categorytheory.zulipchat.com/#narrow/stream/241590-theory.3A-
-- algebraic.20topology.20.26.20homological.20algebra/topic/describing.
-- 20simplicial.20sets/near/260675092
--
-- There's a lot more interesting stuff to say about this situation.
--
-- If we want to understand the category of semisimplicial sets
-- relative to the category of simplicial sets via the left adjoint
-- you mentioned, we should answer three questions: 1) Which
-- simplicial sets lie in the image of this functor?  2) Which
-- morphisms lie in the image of this functor?  3) When do two
-- parallel morphisms of semisimplicial sets become equal when we
-- apply this functor?
--
-- I think, though I haven't carefully checked, that the answers are:
--
-- 1) The simplicial sets in which every face of a nondegenerate simplex is
--    nondegenerate.
-- 2) The morphisms which send nondegenerate simplices to nondegenerate
--    simplices.
-- 3) Only if the maps were already equal, i.e., the functor is faithful.
--
-- There's also a more efficient way to describe what this left
-- adjoint produces, related to the kerodon proposition that Daniel
-- linked to, and using the notion of a "degeneracy operation". A
-- degeneracy operation is an operation taking nn-simplices to
-- mm-simplices for some fixed nn and mm, for which the corresponding
-- map [m]→[n] of Δ is surjective. (So in particular, n≤m.) The
-- operations s_i are the generating degeneracy opaerations, and the
-- degeneracy options are all compositions of the s_is, but quotiented
-- by the simplicial relations involving the s_i.
--
-- The linked proposition says that every simplex of a simplicial set
-- can be expressed as a degeneracy operation applied to a
-- nondegenerate simplex in a unique way.
--
-- Now if we start with a semisimplicial set X, we can describe the
-- "free" simplicial set Y it generates as follows:
--
-- - The simplices of Y are formal applications of a degeneracy
--   operation to a simplex of X.
-- - The structure maps of X are computed as follows. Suppose we want
--   to compute the action of a simplicial operator ff on a formal
--   degeneracy sx. The combined operation fsfs corresponds to some
--   map of Δ which we can refactor as a surjection followed by an
--   injection. Then f(sx) is given by formally applying the
--   degeneracy operator corresponding to the surjection to the value
--   of the face operator corresponding to the injection on x
--   (computed in the semisimplicial set X).
--
-- A more syntactic way to describe the action in terms of the
-- generating face and degenerating operators is:
--
-- - If we want to apply s_i to a formal degeneracy sx, we just form (s_i s) x
-- - If we want to apply d_i to a formal degeneracy sx, then we use
--   the simplicial identities to rewrite d_i s as a composition s' d'
--   s "moving ds to the left". Since we started with a single d_i ,
--   what will happen is that either d_i will pass through all the ss
--   (possibly changing indices in the process) so that d = d_j or the
--   d_i ​ will cancel with some s, so that d = id.  Then we compute x'
--   = d' x in X and form s' x'.
--
-- There is also a way to specify an arbitrary simplicial set
-- in terms of only its nondegenerate simplices and its face maps, but
-- with the caveat that the face of a nondegenerate simplex can be a
-- formal degeneracy of another nondegenerate simplex. The full
-- simplicial structure is recovered by the same process as above
-- except that when we take the face of a nondegenerate simplex (in
-- what would have been X above), it may come as a formal degeneracy
-- to which we have to apply another degeneracy operator to--which is
-- no problem.
--
-- The other caveat is that because of the original question 2, in
-- order to recover the correct maps of simplicial sets, we also need
-- to allow a map to send a nondegenerate simplex to a formal
-- degeneracy in the target simplicial set.
--
-- The program Kenzo uses this representation of simplicial sets.
