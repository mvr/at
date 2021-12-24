{-# LANGUAGE UndecidableInstances #-}

module Math.Topology.SSet where

import Math.Algebra.ChainComplex as CC
  ( ChainComplex (..),
    ChainMorphism (ChainMorphism),
    Combination (Combination),
  )
import qualified Math.Algebra.ChainComplex as CC
  ( LevelwiseFinite (..),
  )

-- NOTE: This could be made much more efficient. This could be
-- flattened so that in a degenerate simplex you have immediate access
-- to the underlying non-degenerate simplex. The list of ints is
-- always strictly decreasing, and so could be stored as a bit mask as
-- is done in Kenzo.  Can use pattern synonyms to make this
-- indistinguishable from what is used currently.
data FormalDegen a
  = NonDegen a
  | Degen Int (FormalDegen a)
  deriving (Eq, Show, Functor)

isDegen :: FormalDegen a -> Bool
isDegen (NonDegen _) = False
isDegen (Degen _ _) = True

type Simplex a = FormalDegen (GeomSimplex a)

class SSet a where
  -- NOTE: Maybe this shouldn't be an associated type, instead just
  -- another parameter to the typeclass

  -- NOTE: Or we could even reverse things, so that GeomSimplex is the
  -- class and SSet is the associated type.
  type GeomSimplex a

  isSimplex :: a -> GeomSimplex a -> Bool
  simplexDim :: a -> GeomSimplex a -> Int
  geomFace :: a -> GeomSimplex a -> Int -> Simplex a

  geomFaces :: a -> GeomSimplex a -> [Simplex a]
  geomFaces a s = fmap (geomFace a s) [0 .. simplexDim a s]

face :: SSet a => a -> Simplex a -> Int -> Simplex a
face a (NonDegen s) i = geomFace a s i
face a (Degen j s) i
  | i < j = Degen (j -1) (face a s i)
  | i > j + 1 = Degen j (face a s (i -1))
  | otherwise = s

degen :: a -> Simplex a -> Int -> Simplex a
degen a (Degen j s) i | i <= j = Degen (j + 1) (degen a s i)
degen a s i = Degen i s

downshift :: a -> Simplex a -> Simplex a
downshift a (NonDegen s) = NonDegen s
downshift a (Degen i s) = Degen (i+1) (downshift a s)

degenList :: a -> Simplex a -> [Int]
degenList a (NonDegen _) = []
degenList a (Degen i s) = i : degenList a s

unDegen :: a -> Simplex a -> [Int] -> Simplex a
unDegen a s [] = s
unDegen a (NonDegen _) js = undefined -- shouldn't happe
unDegen a (Degen i s) (j:js)
  | i == j    = unDegen a s js
  | otherwise = Degen (i - length (j : js)) (unDegen a s (j : js))

-- In this representation, we just need to check that the index is
-- somewhere in the list. (Not necessarily the first thing)
isImageOfDegen :: a -> Simplex a -> Int -> Bool
isImageOfDegen a (NonDegen _) _ = False
isImageOfDegen a (Degen j s) i
  | i == j = True
  | i >  j = False -- We missed it, it can't be further down.
  | otherwise = isImageOfDegen a s i

constantAtVertex :: SSet a => a -> GeomSimplex a -> Int -> Simplex a
constantAtVertex a g 0 = NonDegen g
constantAtVertex a g n = Degen (n -1) $ constantAtVertex a g (n -1)

class SSet a => LevelwiseFinite a where
  -- * `all isSimplex (geomBasis n)`
  geomBasis :: a -> Int -> [GeomSimplex a]

-- https://kerodon.net/tag/00QH
newtype NormalisedChains a = NormalisedChains a
  deriving (Show)

instance (Eq (GeomSimplex a), SSet a) => ChainComplex (NormalisedChains a) where
  type Basis (NormalisedChains a) = GeomSimplex a
  diff (NormalisedChains chs) = ChainMorphism (-1) act
    where
      act v = sum [Combination [(c, s)] | (c, NonDegen s) <- zip signs $ geomFaces chs v]
      signs = cycle [1, -1]

instance (Eq (GeomSimplex a), LevelwiseFinite a) => CC.LevelwiseFinite (NormalisedChains a) where
  dim (NormalisedChains a) i = length (geomBasis a i)
  basis (NormalisedChains a) i = geomBasis a i

class Pointed a where
  basepoint :: a -> GeomSimplex a

-- Reid Barton:
-- https://categorytheory.zulipchat.com/#narrow/stream/241590-theory.3A-algebraic.20topology.20.26.20homological.20algebra/topic/describing.20simplicial.20sets/near/260675092
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
