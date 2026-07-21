{-# LANGUAGE UndecidableInstances #-}

-- | Whitehead-tower stages and homotopy groups of 1-reduced spaces.
module Math.Topology.SSet.Whitehead
  ( HomotopyError (..),
    whiteheadTwist,
    whiteheadStage,
    homotopyGroupsThrough,
    homotopyGroup,
  )
where

import Data.Bifunctor (first)

import Math.Algebra.AbGroupPres
import qualified Math.Algebra.ChainComplex as CC
import Math.Topology.SGrp.KGn.Cocycle
  ( EilenbergMacLane,
    SomeEilenbergMacLane (..),
    coefficientSpace,
    cocycleClassifyingMap,
  )
import Math.Topology.SGrp.Wbar
import Math.Topology.SSet
import Math.Topology.SSet.Effective
import Math.Topology.SSet.TwistedProduct
import Math.ValueCategory.Additive (zero)

whiteheadTwist ::
  (Effective a, EilenbergMacLane g) =>
  a ->
  g ->
  CC.FundamentalCocycle (Model a) ->
  Twist a g
whiteheadTwist a g cocycle =
  pullback
    (Wbar g)
    g
    (canonicalTwist g)
    (cocycleClassifyingMap a g cocycle)

-- | Total space of the principal fibration classified by a fundamental cocycle.
whiteheadStage ::
  (Effective a, EilenbergMacLane g) =>
  a ->
  g ->
  CC.FundamentalCocycle (Model a) ->
  TotalSpace a g
whiteheadStage a g cocycle =
  totalSpace a g (whiteheadTwist a g cocycle)

data HomotopyError
  = InvalidHomotopyDegree Int
  | CocycleError String
  deriving (Eq, Show)

data SomeSpace = forall a.
  (Effective a, CC.FiniteType (Model a)) =>
  SomeSpace a

killHomologyGroup :: Int -> SomeSpace -> Either HomotopyError SomeSpace
killHomologyGroup degree space@(SomeSpace a) = do
  cocycles <- first CocycleError $ CC.fundamentalCocycles (model a) degree
  case cocycles of
    [] -> Right space
    cocycle : _ -> case coefficientSpace (fromIntegral <$> CC.cocycleOrder cocycle) (degree - 1) of
      SomeEilenbergMacLane g ->
        killHomologyGroup degree $ SomeSpace (whiteheadStage a g cocycle)

-- | Compute pi_2 through pi_n by successive Whitehead stages.
homotopyGroupsThrough ::
  (OneReduced a, Effective a, CC.FiniteType (Model a)) =>
  Int ->
  a ->
  Either HomotopyError [(Int, AbGroupPres)]
homotopyGroupsThrough target a
  | target < 2 = Left $ InvalidHomotopyDegree target
  | otherwise = groupsFrom 2 (SomeSpace a)
  where
    groupsFrom degree space@(SomeSpace current) = do
      let group = homology current !! degree
      if degree == target
        then Right [(degree, group)]
        else do
          next <- if group == zero then Right space else killHomologyGroup degree space
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
