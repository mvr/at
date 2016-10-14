{-# LANGUAGE PatternSynonyms #-}
module Math.Algebra.AbGroup.Effective where

import Math.Algebra.AbGroup

--------------------------------------------------------------------------------
-- Following
-- "Effective Algebraic Topology"
--  by Rolf Schön
-- Mem. Amer. Math. Soc. 92 (1991), no. 451, vi+63 pp.

-- A f.g. abelian group, presented as the colimit of an infinite
-- sequence of f.g. abelian groups.
-- TODO: Consider lists instead, as memoization
data Effective = Effective {
  groups    :: Integer -> AbGroup,
  morphisms :: Integer -> Integer -> Morphism,
  monocal   :: Maybe (Integer -> (Integer, Integer)),
  epical    :: Maybe (Integer -> (Integer, Integer)),
  isocal    :: Maybe (Integer -> (Integer, Integer))
}

pattern Monocal gs ms mono <- Effective gs ms (Just mono) _ _ where
  Monocal gs ms mono = Effective gs ms (Just mono) Nothing Nothing

pattern Epical gs ms epi <- Effective gs ms _ (Just epi) _ where
  Epical gs ms epi = Effective gs ms Nothing (Just epi) Nothing

pattern Isocal gs ms iso <- Effective gs ms _ _ (Just iso) where
  Isocal gs ms iso = Effective gs ms (Just iso) (Just iso) (Just iso)

instance Show Effective where
  show (Isocal _ ms isocal) = show $ image $ ms i j
   where (i, j) = isocal 1
  show (Epical _ _ _)  = "<An epical group>"
  show (Monocal _ _ _) = "<A monocal group>"
  show _               = "<An unspecified group>"

autocomposeMorphisms :: (Integer -> Morphism) -> (Integer -> Integer -> Morphism)
autocomposeMorphisms ms i j
  | i == j     = identityMorphism $ domain $ ms i
  | j == i + 1 = ms i
  | otherwise  = autocomposeMorphisms ms (i+1) j `composeMorphisms` ms i

-- Smart constructor for Effective that fills in gaps as necessary.
effectiveGroup
  :: (Integer -> AbGroup)
     -> (Integer -> Integer -> Morphism)
     -> Maybe (Integer -> (Integer, Integer))
     -> Maybe (Integer -> (Integer, Integer))
     -> Maybe (Integer -> (Integer, Integer))
     -> Effective
effectiveGroup gs ms Nothing Nothing (Just isocal)
  = Effective gs ms (Just isocal) (Just isocal) (Just isocal)
effectiveGroup gs ms (Just monocal) (Just epical) Nothing
  = Effective gs ms (Just monocal) (Just epical) (Just isocal)
    where isocal i = let j = fst $ epical i in
                     monocal j
effectiveGroup gs ms mono epi iso
  = Effective gs ms mono epi iso

constantEffectiveGroup :: AbGroup -> Effective
constantEffectiveGroup g
  = Effective (const g) (const $ const $ identityMorphism g)
              (Just idIndices) (Just idIndices) (Just idIndices)
  where idIndices i = (i, i)

zeroEffectiveGroup :: Effective
zeroEffectiveGroup = constantEffectiveGroup zeroGroup

--------------------------------------------------------------------------------
-- Morphisms

data EffectiveMorphism = EffectiveMorphism {
  effectiveDomain             :: Effective,
  effectiveCodomain           :: Effective,
  effectivePointwiseMorphisms :: Integer -> Morphism
}

composeEffectiveMorphisms
  :: EffectiveMorphism -> EffectiveMorphism -> EffectiveMorphism
composeEffectiveMorphisms (EffectiveMorphism _ c f') (EffectiveMorphism d _ f)
  = EffectiveMorphism d c (\i -> f' i `composeMorphisms` f i)

--------------------------------------------------------------------------------
-- The Five Lemma Algorithm

fiveLemma
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
fiveLemma j k l m n f g h = undefined
