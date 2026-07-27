{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | The standard cocycle coordinates for Eilenberg-Mac Lane spaces,
-- decoded into their iterated classifying-space models.
module Math.Topology.SGrp.KGn.Cocycle (
  EilenbergMacLane (..),
  SomeEilenbergMacLane (..),
  iteratedEilenbergMacLane,
  coefficientSpace,
  CocycleCoordinate,
  CocycleFaceValues (..),
  evaluateCocycleFaces,
  cocycleDoldKanMap,
  cocycleClassifyingMap,
)
where

import Data.Bits (complement, countTrailingZeros, popCount)
import Data.Maybe (fromMaybe, mapMaybe)

import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Equivalence (equivalenceForward)
import Math.Algebra.Combination
import Math.Algebra.Group
import Math.Topology.SGrp
import Math.Topology.SGrp.KGn (KZmod2_1 (..), kz1)
import Math.Topology.SGrp.KGn.DoldKan (
  DoldKanKGn (DoldKanKGn),
  DoldKanSurjection (DoldKanSurjection),
  doldKanSurjections,
 )
import qualified Math.Topology.SGrp.KGn.DoldKan as DoldKan
import Math.Topology.SGrp.KGn.NormalizedCocycle (
  CocycleCoordinate,
  CocycleFaceValues (CocycleFaceValues),
  cocycleCoordinateVertices,
  cocycleValuesToDoldKan,
 )
import Math.Topology.SGrp.Wbar
import qualified Math.Topology.SGrp.Wbar as Wbar
import Math.Topology.SGrp.WbarDiscrete
import qualified Math.Topology.SGrp.WbarDiscrete as WbarDiscrete
import Math.Topology.SSet
import Math.Topology.SSet.Effective
import Math.Topology.SSet.NChains

-- | A simplicial Eilenberg-Mac Lane group represented by iterated
-- classifying spaces, together with its cyclic coefficient group.
class
  ( SAb g,
    ZeroReduced g,
    Abelian (CoefficientGroup g)
  ) =>
  EilenbergMacLane g
  where
  type CoefficientGroup g

  emCoefficientGroup :: g -> CoefficientGroup g
  emCoefficientFromInteger :: g -> Integer -> Element (CoefficientGroup g)
  emDegree :: g -> Int
  emSimplexForCoefficient :: g -> Element (CoefficientGroup g) -> Simplex g
  emSimplexFromCocycle :: g -> Int -> [(Int, Element (CoefficientGroup g))] -> Simplex g

instance EilenbergMacLane (WbarDiscrete Z) where
  type CoefficientGroup (WbarDiscrete Z) = Z

  emCoefficientGroup _ = Z
  emCoefficientFromInteger _ = id
  emDegree _ = 1
  emSimplexForCoefficient _ value = WbarDiscrete.normalise Z [value]
  emSimplexFromCocycle _ _ cocycle =
    WbarDiscrete.normalise Z (snd <$> cocycle)

instance EilenbergMacLane (WbarDiscrete Zmod) where
  type CoefficientGroup (WbarDiscrete Zmod) = Zmod

  emCoefficientGroup (WbarDiscrete group) = group
  emCoefficientFromInteger (WbarDiscrete group) = zmodElement group
  emDegree _ = 1
  emSimplexForCoefficient (WbarDiscrete group) value =
    WbarDiscrete.normalise group [value]
  emSimplexFromCocycle (WbarDiscrete group) _ cocycle =
    WbarDiscrete.normalise group (snd <$> cocycle)

instance EilenbergMacLane KZmod2_1 where
  type CoefficientGroup KZmod2_1 = Zmod

  emCoefficientGroup _ = Zmod 2
  emCoefficientFromInteger _ = zmodElement (Zmod 2)
  emDegree _ = 1
  emSimplexForCoefficient _ value =
    fmap length $
      WbarDiscrete.normalise (Zmod 2) [value]
  emSimplexFromCocycle _ _ cocycle =
    fmap length $
      WbarDiscrete.normalise (Zmod 2) (snd <$> cocycle)

instance EilenbergMacLane g => EilenbergMacLane (Wbar g) where
  type CoefficientGroup (Wbar g) = CoefficientGroup g

  emCoefficientGroup (Wbar g) = emCoefficientGroup g
  emCoefficientFromInteger (Wbar g) = emCoefficientFromInteger g
  emDegree (Wbar g) = 1 + emDegree g
  emSimplexForCoefficient p@(Wbar g) value =
    Wbar.normalise g $
      emSimplexForCoefficient g value
        : [constantAt (basepoint g) i | i <- [degree - 2, degree - 3 .. 0]]
    where
      degree = emDegree p
  emSimplexFromCocycle p@(Wbar g) simplexDegree cocycle
    | simplexDegree < emDegree p = constantAt (basepoint p) simplexDegree
    | simplexDegree == emDegree p = case cocycle of
        (_, value) : _ -> emSimplexForCoefficient p value
        [] -> constantAt (basepoint p) simplexDegree
    | otherwise =
        Wbar.normalise g $
          emSimplexFromCocycle g (simplexDegree - 1) oddHalf
            : Wbar.unnormalise g (emSimplexFromCocycle p (simplexDegree - 1) evenHalf)
    where
      (evenHalf, oddHalf) = splitCocycle p cocycle

-- | An effective Eilenberg-Mac Lane space with its concrete iterated
-- classifying-space type hidden.
data SomeEilenbergMacLane
  = forall g.
    ( EilenbergMacLane g,
      Effective g,
      CC.FiniteType (Model g)
    ) =>
    SomeEilenbergMacLane g

iteratedEilenbergMacLane ::
  ( EilenbergMacLane g,
    Effective g,
    CC.FiniteType (Model g)
  ) =>
  Int ->
  g ->
  SomeEilenbergMacLane
iteratedEilenbergMacLane target g
  | emDegree g >= target = SomeEilenbergMacLane g
  | otherwise = iteratedEilenbergMacLane target (Wbar g)

coefficientSpace :: Maybe Int -> Int -> SomeEilenbergMacLane
coefficientSpace Nothing degree = iteratedEilenbergMacLane degree kz1
coefficientSpace (Just 2) degree = iteratedEilenbergMacLane degree KZmod2_1
coefficientSpace (Just order) degree = iteratedEilenbergMacLane degree (WbarDiscrete (Zmod order))

-- This is the coordinate decomposition underlying
-- K(A,n) = Wbar K(A,n-1). Even masks form the Wbar tail; odd masks,
-- after conversion to inhomogeneous coordinates, form its head.
splitCocycle ::
  EilenbergMacLane g =>
  g ->
  [(Int, Element (CoefficientGroup g))] ->
  ( [(Int, Element (CoefficientGroup g))],
    [(Int, Element (CoefficientGroup g))]
  )
splitCocycle g cocycle = (evenHalf, adjust <$> oddHalf)
  where
    coefficients = emCoefficientGroup g
    evenHalf = [(key `div` 2, value) | (key, value) <- cocycle, even key]
    oddHalf = [(key `div` 2, value) | (key, value) <- cocycle, odd key]
    adjust (key, value)
      | even key = (key, prod coefficients value (inv coefficients adjacent))
      | otherwise = (key, value)
      where
        adjacent =
          fromMaybe
            (error "splitCocycle: missing adjacent face")
            (lookup (key + 1) evenHalf)

-- These operators select the independent coordinates in the standard
-- cocycle model of K(A,n). The integer keys are Kenzo's bit-mask convention.
cocycleCoordinateOperators :: Int -> Int -> [(Int, FaceOperator)]
cocycleCoordinateOperators smallDegree highDegree
  | smallDegree == highDegree =
      [(2 ^ highDegree - 1, identityFaceOperator highDegree)]
  | otherwise = fmap makeFace keys
  where
    previous = cocycleCoordinateOperators (smallDegree + 1) highDegree
    keys = [key | key <- [0 .. 2 ^ highDegree - 2], popCount key == smallDegree]
    makeFace key =
      let i = countTrailingZeros (complement key)
          faceIndex = if key == 2 ^ i - 1 then i + 1 else i
          source = fromMaybe (error "cocycleCoordinateOperators: missing recursive face") (lookup (key + 2 ^ i) previous)
       in (key, faceOperatorFace source faceIndex)

bitmaskCoordinateFaces :: SSet a => a -> Int -> Int -> GeomSimplex a -> [(Int, Simplex a)]
bitmaskCoordinateFaces a smallDegree highDegree simplex =
  [ (key, applyFaceOperator a operator (NonDegen simplex))
  | (key, operator) <- cocycleCoordinateOperators smallDegree highDegree
  ]

cocycleCoordinateFaces ::
  SSet a =>
  a ->
  Int ->
  Int ->
  GeomSimplex a ->
  [(CocycleCoordinate, Simplex a)]
cocycleCoordinateFaces a n q simplex =
  coordinateFace <$> doldKanSurjections n q
  where
    coordinateFace (DoldKanSurjection _ coordinate) =
      ( coordinate,
        applyFaceOperator
          a
          (FaceOperator q (cocycleCoordinateVertices coordinate))
          (NonDegen simplex)
      )

-- | Evaluate a cocycle on the independent normalized faces of each source
-- simplex.
evaluateCocycleFaces ::
  ( SSet a,
    Group c,
    Eq (Element c)
  ) =>
  a ->
  c ->
  CC.Cocycle (NChains a) c ->
  GeomSimplex a ->
  CocycleFaceValues (Element c)
evaluateCocycleFaces a c cocycle@(CC.Cocycle (CC.Cochain n _)) simplex =
  CocycleFaceValues q $
    mapMaybe evaluateFace (cocycleCoordinateFaces a n q simplex)
  where
    q = geomSimplexDim a simplex
    evaluateFace (_, Degen _ _) = Nothing
    evaluateFace (coordinate, NonDegen faceSimplex) =
      let value = CC.cocycleOnBasis cocycle (BasisSimplex faceSimplex)
       in if value == unit c
            then Nothing
            else Just (coordinate, value)

-- | The inverse Dold--Kan image of a cocycle.
cocycleDoldKanMap ::
  ( Effective a,
    Abelian c,
    Eq (Element c)
  ) => a -> c -> CC.Cocycle (Model a) c -> Morphism a (DoldKanKGn c)
cocycleDoldKanMap a c cocycle = Morphism $ \simplex ->
  DoldKan.normalise $
    cocycleValuesToDoldKan
      (DoldKanKGn n c)
      (evaluateCocycleFaces a c sourceCocycle simplex)
  where
    sourceCocycle =
      CC.pullbackCocycle c (equivalenceForward (eff a)) cocycle
    n = CC.cocycleDegree sourceCocycle

evaluateFundamentalCocycle ::
  CC.FundamentalCocycle (NChains a) ->
  Simplex a ->
  Integer
evaluateFundamentalCocycle _ (Degen _ _) = 0
evaluateFundamentalCocycle cocycle (NonDegen simplex) =
  fromIntegral $
    coeffOf
      (CC.cocycleMorphism cocycle `CC.onBasis` BasisSimplex simplex)
      ()

-- | The classifying map to K(A,n) = Wbar K(A,n-1) represented by a
-- fundamental cocycle on the effective model of the source.
cocycleClassifyingMap ::
  (Effective a, EilenbergMacLane g) =>
  a ->
  g ->
  CC.FundamentalCocycle (Model a) ->
  Morphism a (Wbar g)
cocycleClassifyingMap a g cocycle = Morphism $ \simplex ->
  let simplexDegree = geomSimplexDim a simplex
      values
        | simplexDegree < degree = []
        | otherwise =
            [ (key, emCoefficientFromInteger (Wbar g) $ evaluateFundamentalCocycle sourceCocycle faceSimplex)
            | (key, faceSimplex) <- bitmaskCoordinateFaces a degree simplexDegree simplex
            ]
   in emSimplexFromCocycle (Wbar g) simplexDegree values
  where
    sourceCocycle =
      CC.pullbackFundamentalCocycle (equivalenceForward (eff a)) cocycle
    degree = CC.fundamentalCocycleDegree sourceCocycle
