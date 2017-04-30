{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Math.Algebra.AbGroup.Effective where

import Math.Algebra.AbGroup
import Math.ValueCategory
import Math.ValueCategory.Abelian

--------------------------------------------------------------------------------
-- Following
-- "Effective Algebraic Topology"
--  by Rolf Schön
-- Mem. Amer. Math. Soc. 92 (1991), no. 451, vi+63 pp.

-- A f.g. abelian group, presented as the colimit of an infinite
-- sequence of f.g. abelian groups.
-- TODO: Consider intmap memo instead
data Effective = Effective {
  groups    :: Integer -> AbGroup,
  morphisms :: Integer -> Integer -> AbMorphism,
  monocal   :: Maybe (Integer -> (Integer, Integer)),
  epical    :: Maybe (Integer -> (Integer, Integer))
}

pattern Monocal gs ms mono <- Effective gs ms (Just mono) _ where
  Monocal gs ms mono = Effective gs ms (Just mono) Nothing

pattern Epical gs ms epi <- Effective gs ms _ (Just epi) where
  Epical gs ms epi = Effective gs ms Nothing (Just epi)

pattern Isocal gs ms iso <- ((\i@(Effective gs ms mono epi) -> (gs, ms, isocal i)) -> (gs, ms, Just iso)) where
  Isocal gs ms iso = Effective gs ms (Just iso) (Just iso)

isocalFunc :: (Integer -> (Integer, Integer))
           -> (Integer -> (Integer, Integer))
           -> (Integer -> (Integer, Integer))
isocalFunc mono epi i = let j = fst $ epi i in mono j

isocal :: Effective -> Maybe (Integer -> (Integer, Integer))
isocal (Effective gs ms mmono mepi) = do
  mono <- mmono
  epi  <- mepi
  return (isocalFunc mono epi)

instance Show Effective where
  show (Isocal _ ms iso) = show $ imageObject $ ms i j
    where (i, j) = iso 1
  show (Epical _ _ _)  = "<An epical group>"
  show (Monocal _ _ _) = "<A monocal group>"
  show _               = "<A totally unknown group>"

autocomposeMorphisms :: (Integer -> AbMorphism) -> (Integer -> Integer -> AbMorphism)
autocomposeMorphisms ms i j
  | i == j     = vid $ domain $ ms i
  | j == i + 1 = ms i
  | otherwise  = autocomposeMorphisms ms (i+1) j .* ms i

constantEffectiveGroup :: AbGroup -> Effective
constantEffectiveGroup g
  = Isocal (const g) (const $ const $ vid g) idIndices
  where idIndices i = (i, i)

zeroEffectiveGroup :: Effective
zeroEffectiveGroup = constantEffectiveGroup zero

--------------------------------------------------------------------------------
-- Morphisms

data EffectiveMorphism = EffectiveMorphism {
  effectiveDomain             :: Effective,
  effectiveCodomain           :: Effective,
  effectivePointwiseMorphisms :: Integer -> AbMorphism
}

composeEffectiveMorphisms
  :: EffectiveMorphism -> EffectiveMorphism -> EffectiveMorphism
composeEffectiveMorphisms (EffectiveMorphism _ c f') (EffectiveMorphism d _ f)
  = EffectiveMorphism d c (\i -> f' i .* f i)

--------------------------------------------------------------------------------
-- The Five Lemma Algorithm

fiveLemmaEpical
  :: Effective
     -> Effective
     -> Effective
     -> Effective
     -> Effective
     -> EffectiveMorphism
     -> EffectiveMorphism
     -> EffectiveMorphism
     -> EffectiveMorphism
     -> Effective
fiveLemmaEpical j k l m n f g h i = undefined

-- fiveLemma
--   :: Effective
--      -> Effective
--      -> Effective
--      -> Effective
--      -> Effective
--      -> EffectiveMorphism
--      -> EffectiveMorphism
--      -> EffectiveMorphism
--      -> EffectiveMorphism
--      -> Effective
-- fiveLemma j k l m n f g h i = undefined
