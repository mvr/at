{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Comparison between the standard single-degree inverse Dold--Kan model
-- and the concrete iterated @Wbar@ models of abelian Eilenberg--Mac Lane
-- spaces.
module Math.Topology.SGrp.KGn.DoldKan.Wbar (
  DoldKanWbarModel (
    CoefficientGroup,
    emCoefficientGroup,
    emDegree,
    emSimplexFromDoldKan
  ),
  doldKanModel,
  doldKanComparison,
)
where

import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)

import Math.Algebra.Group
import Math.Topology.SGrp
import Math.Topology.SGrp.KGn (KZmod2_1)
import Math.Topology.SGrp.KGn.DoldKan (
  DoldKanGeomSimplex (DoldKanGeomSimplex),
  DoldKanKGn (DoldKanKGn),
  DoldKanSimplex (DoldKanSimplex),
 )
import Math.Topology.SGrp.Wbar
import qualified Math.Topology.SGrp.Wbar as Wbar
import Math.Topology.SGrp.WbarDiscrete
import qualified Math.Topology.SGrp.WbarDiscrete as WbarDiscrete
import Math.Topology.SSet
import Math.Topology.SSet.Surjection

-- | A concrete simplicial Eilenberg--Mac Lane group together with its abelian
-- coefficient group and comparison from the standard Dold--Kan model.
class
  ( SAb g,
    ZeroReduced g,
    Abelian (CoefficientGroup g),
    Ord (Element (CoefficientGroup g))
  ) =>
  DoldKanWbarModel g
  where
  type CoefficientGroup g

  emCoefficientGroup :: g -> CoefficientGroup g
  emDegree :: g -> Int
  emSimplexFromDoldKan ::
    g ->
    GeomSimplex (DoldKanKGn (CoefficientGroup g)) ->
    Simplex g

denseDegreeOneValues ::
  (Group c) =>
  c ->
  DoldKanGeomSimplex (Element c) ->
  [Element c]
denseDegreeOneValues
  c
  (DoldKanGeomSimplex (DoldKanSimplex degree summands)) =
    valueAt <$> surjections degree 1
    where
      valueAt coordinate =
        fromMaybe (unit c) (lookup coordinate summands)

instance
  (Abelian c, Ord (Element c)) =>
  DoldKanWbarModel (WbarDiscrete c)
  where
  type CoefficientGroup (WbarDiscrete c) = c

  emCoefficientGroup (WbarDiscrete c) = c
  emDegree _ = 1
  emSimplexFromDoldKan (WbarDiscrete c) simplex =
    WbarDiscrete.normalise c (denseDegreeOneValues c simplex)

instance DoldKanWbarModel KZmod2_1 where
  type CoefficientGroup KZmod2_1 = Zmod

  emCoefficientGroup _ = Zmod 2
  emDegree _ = 1
  emSimplexFromDoldKan _ simplex =
    length <$> WbarDiscrete.normalise (Zmod 2) (denseDegreeOneValues (Zmod 2) simplex)

instance (DoldKanWbarModel g) => DoldKanWbarModel (Wbar g) where
  type CoefficientGroup (Wbar g) = CoefficientGroup g

  emCoefficientGroup (Wbar g) = emCoefficientGroup g
  emDegree (Wbar g) = 1 + emDegree g
  emSimplexFromDoldKan
    p@(Wbar g)
    simplex@(DoldKanGeomSimplex (DoldKanSimplex degree _))
      | degree < emDegree p = constantAt (geomBasepoint p) degree
      | otherwise =
          Wbar.normalise g $
            emSimplexFromDoldKan g headValues
              : Wbar.unnormalise g (emSimplexFromDoldKan p tailValues)
      where
        (headValues, tailValues) = splitDoldKan simplex

-- | The standard Dold--Kan model corresponding to an iterated
-- classifying-space model.
doldKanModel :: (DoldKanWbarModel g) => g -> DoldKanKGn (CoefficientGroup g)
doldKanModel target =
  DoldKanKGn (emDegree target) (emCoefficientGroup target)

-- | Compare the standard Dold--Kan model of @K(A,n)@ with a concrete
-- iterated classifying-space model.
doldKanComparison ::
  (DoldKanWbarModel g) =>
  g ->
  Morphism (DoldKanKGn (CoefficientGroup g)) g
doldKanComparison target = Morphism (emSimplexFromDoldKan target)

-- This is the standard surjection decomposition underlying
-- K(A,n) = Wbar K(A,n-1). Surjections which change value at the first edge
-- form the Wbar head; those which repeat there form its tail.
splitDoldKan ::
  DoldKanGeomSimplex e ->
  ( DoldKanGeomSimplex e,
    DoldKanGeomSimplex e
  )
splitDoldKan (DoldKanGeomSimplex (DoldKanSimplex degree summands)) =
  ( DoldKanGeomSimplex (DoldKanSimplex (degree - 1) headValues),
    DoldKanGeomSimplex (DoldKanSimplex (degree - 1) tailValues)
  )
  where
    (headValues, tailValues) = partitionEithers (splitSummand <$> summands)
    splitSummand (Surjection _ ts, n) =
      case ts of
        0 : rest -> Left (reindex rest, n)
        _ -> Right (reindex ts, n)
    reindex ts = Surjection (degree - 1) (subtract 1 <$> ts)
