{-# LANGUAGE UndecidableInstances #-}

-- | Whitehead-tower stages and homotopy groups of 1-reduced spaces.
module Math.Topology.SSet.Whitehead (
  HomotopyError (..),
  whiteheadTwist,
  whiteheadStage,
  homotopyGroupsThrough,
  homotopyGroup,
)
where

import Math.Algebra.AbGroupPres
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.Group (Z, Zmod (..))
import Math.Topology.SGrp.KGn (KZmod2_1 (..), kz1)
import Math.Topology.SGrp.KGn.DoldKan.Cocycle (cocycleClassifyingMap)
import Math.Topology.SGrp.KGn.DoldKan.Wbar (
  DoldKanWbarModel (CoefficientGroup, emDegree),
 )
import Math.Topology.SGrp.Wbar
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet
import Math.Topology.SSet.Effective
import Math.Topology.SSet.TwistedProduct
import Math.ValueCategory (Arrow)
import qualified Math.ValueCategory.Abelian as Abelian
import Math.ValueCategory.Additive (zero)

whiteheadTwist ::
  (Effective a, DoldKanWbarModel g) =>
  a ->
  g ->
  CC.Cocycle (Model a) (CoefficientGroup g) ->
  Twist a g
whiteheadTwist a g cocycle =
  pullback
    (Wbar g)
    g
    (canonicalTwist g)
    (cocycleClassifyingMap a g cocycle)

-- | Total space of the principal fibration classified by a cocycle.
whiteheadStage ::
  (Effective a, DoldKanWbarModel g) =>
  a ->
  g ->
  CC.Cocycle (Model a) (CoefficientGroup g) ->
  TotalSpace a g
whiteheadStage a g cocycle =
  totalSpace a g (whiteheadTwist a g cocycle)

data HomotopyError
  = InvalidHomotopyDegree Int
  deriving (Eq, Show)

data SomeSpace
  = forall a.
    (Effective a, CC.FiniteType (Model a)) =>
    SomeSpace a (Model a) [Arrow AbGroupPres]

someSpace ::
  (Effective a, CC.FiniteType (Model a)) =>
  a ->
  SomeSpace
someSpace a = SomeSpace a effectiveModel (CC.chainDiffs effectiveModel)
  where
    effectiveModel = model a

data SomeEilenbergMacLane c
  = forall g.
    ( DoldKanWbarModel g,
      CoefficientGroup g ~ c,
      Effective g,
      CC.FiniteType (Model g)
    ) =>
    SomeEilenbergMacLane g

iteratedEilenbergMacLane ::
  ( DoldKanWbarModel g,
    Effective g,
    CC.FiniteType (Model g)
  ) =>
  Int ->
  g ->
  SomeEilenbergMacLane (CoefficientGroup g)
iteratedEilenbergMacLane target g =
  case compare (emDegree g) target of
    LT -> iteratedEilenbergMacLane target (Wbar g)
    EQ -> SomeEilenbergMacLane g
    GT -> error "iteratedEilenbergMacLane: target below starting degree"

integralCoefficientSpace :: Int -> SomeEilenbergMacLane Z
integralCoefficientSpace degree = iteratedEilenbergMacLane degree kz1

modularCoefficientSpace :: Zmod -> Int -> SomeEilenbergMacLane Zmod
modularCoefficientSpace c@(Zmod order) degree
  | order == 2 = iteratedEilenbergMacLane degree KZmod2_1
  | otherwise = iteratedEilenbergMacLane degree (WbarDiscrete c)

killHomologyGroup :: Int -> SomeSpace -> SomeSpace
killHomologyGroup degree space@(SomeSpace a effectiveModel differentials) =
  case CC.fundamentalCocyclesWithDiffs effectiveModel degree outgoing incoming of
    [] -> space
    CC.IntegralFundamentalCocycle cocycle : _ ->
      case integralCoefficientSpace (degree - 1) of
        SomeEilenbergMacLane g ->
          killHomologyGroup degree $ someSpace (whiteheadStage a g cocycle)
    CC.ModularFundamentalCocycle c cocycle : _ ->
      case modularCoefficientSpace c (degree - 1) of
        SomeEilenbergMacLane g ->
          killHomologyGroup degree $ someSpace (whiteheadStage a g cocycle)
  where
    outgoing = differentials !! degree
    incoming = differentials !! (degree + 1)

-- | Compute pi_2 through pi_n by successive Whitehead stages.
homotopyGroupsThrough ::
  (OneReduced a, Effective a, CC.FiniteType (Model a)) =>
  Int ->
  a ->
  Either HomotopyError [(Int, AbGroupPres)]
homotopyGroupsThrough target a
  | target < 2 = Left $ InvalidHomotopyDegree target
  | otherwise = groupsFrom 2 (someSpace a)
  where
    groupsFrom degree space@(SomeSpace _ _ differentials) = do
      let group =
            Abelian.homology
              (differentials !! (degree + 1))
              (differentials !! degree)
      if degree == target
        then Right [(degree, group)]
        else do
          let next = if group == zero then space else killHomologyGroup degree space
          ((degree, group) :) <$> groupsFrom (degree + 1) next

-- | Compute a single homotopy group of a 1-reduced simplicial set.
homotopyGroup ::
  (OneReduced a, Effective a, CC.FiniteType (Model a)) =>
  Int ->
  a ->
  Either HomotopyError AbGroupPres
homotopyGroup degree a = do
  groups <- homotopyGroupsThrough degree a
  maybe (Left $ InvalidHomotopyDegree degree) Right (lookup degree groups)
