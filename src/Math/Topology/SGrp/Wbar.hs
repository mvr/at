{-# LANGUAGE UndecidableInstances #-}

-- | Classifying spaces for simplicial groups
-- Wbar : sGrp -> 0-reduced sSet_*
-- See https://ncatlab.org/nlab/show/simplicial+classifying+space
-- In the Kenzo source, this is spread over
-- classifying-spaces.lisp, classifying-spaces-dvf.lisp, cl-space-efhm.lisp
-- Also anromero/resolutions.lisp in the fork
module Math.Topology.SGrp.Wbar where

import Data.List (intersect)
import Math.Topology.SGrp
import Math.Topology.SSet
import Math.Topology.SSet.DVF
import Math.Topology.SSet.Effective
import Math.Topology.SSet.Morphism
import Math.Topology.SSet.Product

newtype Wbar g = Wbar g
  deriving Show

newtype WbarSimplex a = WbarSimplex a
  deriving Show via a
  deriving Functor

deriving instance (Eq g) => Eq (WbarSimplex g)

-- TODO: there are probably efficient algorithms for this in terms of bit fields.
-- 1. Create a bit field marking which positions are the unit
-- 2. Intersect this with the shifted degeneracy operators for each entry
-- 3. Delete all the surviving units and bit-extract every degeneracy operator appropriately
filterCandidates :: (Pointed g) => g -> [Simplex g] -> [Int] -> Int -> [Int]
filterCandidates g ss [] j = []
filterCandidates g [] cs@(ct : crest) j = undefined -- can't happen
filterCandidates g ss@(st : srest) cs@(ct : crest) j
  | j > ct = cs
  | j `elem` cs =
    if isUnit g st
      then ct : filterCandidates g srest crest (j + 1)
      else filterCandidates g srest crest (j + 1)
  | otherwise = filterCandidates g srest (cs `intersect` fmap (+ j) (degenList st)) (j + 1)

downshiftList :: [Int] -> [Int]
downshiftList [] = []
downshiftList [0] = []
downshiftList (i : is) = (i - 1) : downshiftList is

extractDegens :: g -> [Simplex g] -> [Int] -> [Simplex g]
extractDegens g s [] = s
extractDegens g [] cs = []
extractDegens g (s : u : ss) cs
  | last cs == 0 = unDegen s cs : extractDegens g ss (downshiftList cs)
extractDegens g (s : ss) cs
  | otherwise = unDegen s cs : extractDegens g ss (downshiftList cs)

normalise :: (Pointed g) => g -> [Simplex g] -> Simplex (Wbar g)
normalise g [] = NonDegen $ WbarSimplex []
normalise g (s : ss)
  | isUnit g s = degen (normalise g ss) 0
  | otherwise =
    let candidates = degenList s
        successes = filterCandidates g ss candidates 0
     in foldl degen (NonDegen $ WbarSimplex $ extractDegens g (s : ss) successes) (fmap (1 +) successes)

insertUnit :: (Pointed g) => g -> Int -> Int -> [Simplex g] -> [Simplex g]
insertUnit g j 0 ss = constantAt (basepoint g) (length ss) : ss
insertUnit g j i (s : ss) = degen s j : insertUnit g j (i -1) ss
insertUnit g j i _ = error "insertUnit: impossible"

unnormalise :: (Pointed g) => g -> Simplex (Wbar g) -> [Simplex g]
unnormalise g (NonDegen (WbarSimplex gs)) = gs
unnormalise g (Degen i s) = insertUnit g i i (unnormalise g s)

instance (SGrp g) => SSet (Wbar g) where
  -- A non-degenerate simplex is a list of simplices of `g`
  -- (Wbar G)_n = G_n-1 x xG_n-2 x ... x G_0
  -- meeting a slightly complicated condition on whether the list
  -- contains a unit, and the things proceding it are all degeneracies
  type GeomSimplex (Wbar g) = WbarSimplex [Simplex g]

  isGeomSimplex (Wbar g) (WbarSimplex ss) = normalise g ss == NonDegen (WbarSimplex ss) && all (isSimplex g) ss

  geomSimplexDim _ (WbarSimplex ss) = length ss

  geomFace _ (WbarSimplex []) _ = undefined
  -- TODO: need to make sure this matches with Kenzo's conventions,
  -- multiplying on which side (for abelian groups of course it
  -- doesn't matter)
  geomFace (Wbar g) (WbarSimplex ss) i = normalise g (underlying ss i)
    where
      underlying ss i
        | i == 0 = tail ss
        | i == 1 && length ss == 1 = []
        | i == 1 =
          let (s : s' : rest) = ss
           in (prodMor g `onSimplex` prodNormalise (face g s 0, s')) : rest
        | otherwise =
          let (s : rest) = ss
           in (face g s (i - 1)) : underlying rest (i - 1)

instance SGrp g => Pointed (Wbar g) where
  basepoint (Wbar g) = WbarSimplex []

instance (SGrp g) => ZeroReduced (Wbar g)

instance (SGrp g, ZeroReduced g) => OneReduced (Wbar g) -- Not a typo!

instance (SGrp g, ZeroReduced g, FiniteType g) => FiniteType (Wbar g) where
  geomBasis (Wbar g) n =
    filter (isGeomSimplex (Wbar g)) $ fmap WbarSimplex $ sequence $ allSimplices g <$> reverse [0 .. (n - 1)]


instance (SAb g) => SGrp (Wbar g) where
  -- TODO: can be more efficient, everywhere there is a degeneracy
  -- there is no need to actually compute the product.
  prodMor (Wbar g) = Morphism $ \(gs1, gs2) ->
    normalise g $
      (onSimplex (prodMor g) . prodNormalise)
        <$> zip (unnormalise g gs1) (unnormalise g gs2)

  invMor (Wbar g) = Morphism $ NonDegen . fmap (fmap (invMor g `onSimplex`))

instance (SAb g) => SAb (Wbar g)

-- instance (SGrp g) => Kan (Wbar g)

-- Kenzo implements this via DVF when `g` is a 0-reduced simplicial
-- abelian group. This should be enough to compute homotopy groups of
-- 1-reduced simplicial sets, as the K(G,n)s involved should all be of
-- that type.

-- Other simplicial groups will need the more complicated method
-- described in serre.lisp and cl-space-efhm.lisp


instance (SAb g, ZeroReduced g) => DVF (Wbar g) where
  vf = undefined

instance (SGrp g, ZeroReduced g, FiniteType g) => Effective (Wbar g)

--   type Model (Wbar g) = Bar (Model g)
