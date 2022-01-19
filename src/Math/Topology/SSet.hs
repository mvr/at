module Math.Topology.SSet where

-- NOTE: This should be made much more efficient. First, it could be
-- flattened so that in a degenerate simplex you have immediate access
-- to the underlying non-degenerate simplex. Also, the list of ints is
-- always strictly decreasing, and so could be stored as a bit mask as
-- is done in Kenzo.  Can use pattern synonyms to make this
-- indistinguishable from what is used currently, but a lot of the
-- algorithms could then be done using bit operations.
data FormalDegen a
  = NonDegen a
  | Degen Int (FormalDegen a)
  deriving (Eq, Show, Functor)

isDegen :: FormalDegen a -> Bool
isDegen (NonDegen _) = False
isDegen (Degen _ _) = True

underlyingGeom :: FormalDegen a -> a
underlyingGeom (NonDegen s) = s
underlyingGeom (Degen _ s) = underlyingGeom s

degen :: FormalDegen a -> Int -> FormalDegen a
degen (Degen j s) i | i <= j = Degen (j + 1) (degen s i)
degen s i = Degen i s

degenList :: FormalDegen a -> [Int]
degenList (NonDegen _) = []
degenList (Degen i s) = i : degenList s

degenCount :: FormalDegen a -> Int
degenCount (NonDegen _) = 0
degenCount (Degen i s) = 1 + degenCount s

-- In this representation, we just need to check that the index is
-- somewhere in the list. (Not necessarily the first thing)
isImageOfDegen :: FormalDegen a -> Int -> Bool
isImageOfDegen (NonDegen _) _ = False
isImageOfDegen (Degen j s) i
  | i == j = True
  | i >  j = False -- We missed it, it can't be further down.
  | otherwise = isImageOfDegen s i

constantAt :: a -> Int -> FormalDegen a
constantAt a 0 = NonDegen a
constantAt a n = Degen (n - 1) $ constantAt a (n -1)

-- The following are dangerous and only make sense in certain situations.
downshiftN :: Int -> FormalDegen a -> FormalDegen a
downshiftN n (NonDegen s) = NonDegen s
downshiftN n (Degen i s) = Degen (i + n) (downshift s)

downshift :: FormalDegen a -> FormalDegen a
downshift = downshiftN 1

unDegen :: FormalDegen a -> [Int] -> FormalDegen a
unDegen s [] = s
unDegen (NonDegen _) js = undefined -- shouldn't happen
unDegen (Degen i s) (j : js)
  | i == j = unDegen s js
  | otherwise = Degen (i - length (j : js)) (unDegen s (j : js))

type Simplex a = FormalDegen (GeomSimplex a)

class SSet a where
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

  -- TODO: for efficiency
  -- nonDegenFaces :: a -> GeomSimplex a -> [(Int, Simplex a)]

isSimplex :: SSet a => a -> Simplex a -> Bool
isSimplex a (NonDegen s) = isGeomSimplex a s
isSimplex a (Degen _ s) = isSimplex a s

simplexDim :: SSet a => a -> Simplex a -> Int
simplexDim a (NonDegen s) = geomSimplexDim a s
simplexDim a (Degen i s) = 1 + simplexDim a s

face :: SSet a => a -> Simplex a -> Int -> Simplex a
face a (NonDegen s) i = geomFace a s i
face a (Degen j s) i
  | i < j = Degen (j -1) (face a s i)
  | i > j + 1 = Degen j (face a s (i -1))
  | otherwise = s

frontFace :: SSet a => a -> Simplex a ->  Simplex a
frontFace a s = face a s 0

backFace :: SSet a => a -> Simplex a -> Simplex a
backFace a s = face a s (simplexDim a s)

class SSet a => FiniteType a where
  -- * `all isSimplex (geomBasis n)`
  geomBasis :: a -> Int -> [GeomSimplex a]

class Pointed a where
  basepoint :: a -> GeomSimplex a

-- | SSet has unique 0-simplex.
class Pointed a => ZeroReduced a

-- | SSet has no non-degenerate 1-simplices.
class ZeroReduced a => OneReduced a

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
