{-# LANGUAGE UndecidableInstances #-}

-- | Classifying spaces for simplicial groups
-- Wbar : sGrp -> 0-reduced sSet_*
-- See https://ncatlab.org/nlab/show/simplicial+classifying+space
-- In the Kenzo source, this is spread over
-- classifying-spaces.lisp, cl-space-efhm.lisp and anromero/resolutions.lisp
module Math.Topology.SSet.Classifying where

import Math.Topology.SSet
import Math.Topology.SSet.Morphism
import Math.Topology.SSet.Product
-- import Math.Topology.SSet.Effective
import Math.Topology.SGrp
import Data.List (intersect)

newtype Classifying g = Classifying g

-- TODO: inefficient
filterCandidates :: (Pointed g, Eq (GeomSimplex g)) => g -> [Simplex g] -> [Int] -> Int -> [Int]
filterCandidates g ss [] j = []
filterCandidates g [] cs@(ct:crest) j = undefined -- can't happen
filterCandidates g ss@(st:srest) cs@(ct:crest) j
  | j > ct      = cs
  | j `elem` cs = if isUnit g st then
                    ct : filterCandidates g srest crest (j+1)
                  else
                    filterCandidates g srest crest (j+1)
  | otherwise   = filterCandidates g srest (cs `intersect` fmap (+j) (degenList g st)) (j+1)


downshiftList :: [Int] -> [Int]
downshiftList [] = []
downshiftList [0] = []
downshiftList (i : is) = (i-1) : downshiftList is

extractDegens :: g -> [Simplex g] -> [Int] -> [Simplex g]
extractDegens g [] cs = []
extractDegens g (s : ss) cs
  | last cs == 0 = extractDegens g ss (downshiftList cs)
  | otherwise    = unDegen g s cs : extractDegens g ss (downshiftList cs)

normalise :: (Pointed g, Eq (GeomSimplex g)) => g -> [Simplex g] -> Simplex (Classifying g)
normalise g [] = NonDegen []
normalise g (s:ss)
  | isUnit g s = degen (Classifying g) (normalise g ss) 0
  | otherwise = let candidates = degenList g s
                    successes = filterCandidates g ss candidates 1
                in foldl (degen (Classifying g)) (NonDegen $ extractDegens g (s:ss) successes) successes

instance (SGrp g, Eq (GeomSimplex g)) => SSet (Classifying g) where
  -- A non-degenerate simplex is a list of simplices of `g`
  -- (Wbar G)_n = G_n-1 x G_n-1 x ... x G_0
  -- meeting a slightly complicated condition on whether the list
  -- contains a unit, and the things proceding it are all degeneracies
  type GeomSimplex (Classifying g) = [Simplex g]

  isSimplex (Classifying g) ss = undefined -- not (any (isUnit g) ss)

  simplexDim _ ss = length ss

  geomFace _ [] _ = undefined
  geomFace (Classifying g) ss i = normalise g (underlying ss i)
    where underlying ss i
            | i == 0 = tail ss
            | i == 1 && length ss == 1 = []
            | i == 1 = let (s:s':rest) = ss in
                         (prod g `mapSimplex` prodNormalise (Product g g) (face g s 0, s')) : rest
            | otherwise = let (s:rest) = ss in
                            (face g s (i-1)) : underlying rest (i-1)

instance SGrp g => Pointed (Classifying g) where
  basepoint (Classifying g) = []

-- Eventually:
-- instance (SGrp g, Effective g) => Effective (Classifying g)
--   type Model (Classifying g) = Bar (Model g)

instance (SAb g, Eq (GeomSimplex g)) => SGrp (Classifying g) where

instance (SAb g, Eq (GeomSimplex g)) => SAb (Classifying g)

-- instance (SGrp g) => Kan (Classifying g)
