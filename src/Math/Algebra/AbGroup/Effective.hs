{-# LANGUAGE TypeFamilies #-}
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

instance ValueCategory Effective where
  type Morphism Effective = EffectiveMorphism

  vid e = EffectiveMorphism e e (\i -> vid $ groups e i)

  domain = effectiveDomain
  codomain = effectiveCodomain

  (EffectiveMorphism _ c f') .* (EffectiveMorphism d _ f)
    = EffectiveMorphism d c (\i -> f' i .* f i)

--------------------------------------------------------------------------------
-- The Five Lemma Algorithm

-- The setup is an exact sequence
-- J -f-> K -g-> L -h-> M -i-> N
-- We are trying to determine epi/monocals for l


-- TODO: A different ordering may be more efficient
enumeratePairs :: [(Integer, Integer)]
enumeratePairs = [ (m - n, n) | m <- [0..], n <- [0..m] ]

enumeratePairsFrom :: (Integer -> Integer) -> [(Integer, Integer)]
enumeratePairsFrom start = do
  m <- [0..]
  let mStart = start m
  n <- [mStart .. (m + mStart)]
  return (m - n, n)

-- enumeratePairsFrom :: (Integer, Integer) -> [(Integer, Integer)]
-- enumeratePairsFrom (a, b) = fmap (\(i, j) -> (i + a, j + b)) enumeratePairs

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
     -> Maybe (Integer -> (Integer, Integer))
fiveLemmaEpical
  _
  (Epical kgs kms kepi)
  (Effective lgs lms _ _)
  (Epical mgs mms mepi)
  (Monocal ngs nms nmono)
  _
  (EffectiveMorphism _ _ g)
  (EffectiveMorphism _ _ h)
  (EffectiveMorphism _ _ i)
  =
  let
      start j = maximum [kl, ml, nl]
        where (ke, kl) = kepi j
              (me, ml) = mepi ke
              (ne, nl) = nmono me

      -- K(ke(j), k) -> L(ke(j), k) -> M(me(ke(j)), k) -> N(ne(me(ke(j))), k)
      toComplex (j, k) = [ imageMorphism (kms ke k) (lms ke k) (g k)
                         , imageMorphism (lms ke k) (mms me k) (h k)
                         , imageMorphism (mms me k) (nms ne k) (i k)]
        where (ke, kl) = kepi j
              (me, ml) = mepi ke
              (ne, nl) = nmono me

      workingSequence i = head $ filter (isExactSequence . toComplex) $ enumeratePairsFrom start

    in Just $ \i -> let (j, k) = workingSequence i in (fst $ kepi j, k)
fiveLemmaEpical _ _ _ _ _ _ _ _ _ = Nothing

fiveLemmaMonocal
  :: Effective
     -> Effective
     -> Effective
     -> Effective
     -> Effective
     -> EffectiveMorphism
     -> EffectiveMorphism
     -> EffectiveMorphism
     -> EffectiveMorphism
     -> Maybe (Integer -> (Integer, Integer))
fiveLemmaMonocal
  (Epical jgs jms jepi)
  (Monocal kgs kms kmono)
  (Effective lgs lms _ _)
  (Monocal mgs mms mmono)
  _
  (EffectiveMorphism _ _ f)
  (EffectiveMorphism _ _ g)
  (EffectiveMorphism _ _ h)
  _
  =
  let
      start j = maximum [jl, kl, ml]
        where (je, jl) = jepi j
              (ke, kl) = kmono je
              (me, ml) = mmono ke

      toComplex (j, k) = [ imageMorphism (jms je k) (kms ke k) (f k)
                         , imageMorphism (kms ke k) (lms ke k) (g k)
                         , imageMorphism (lms ke k) (mms me k) (h k)]
        where (je, jl) = jepi j
              (ke, kl) = kmono je
              (me, ml) = mmono ke

      workingSequence i = head $ filter (isExactSequence . toComplex) $ enumeratePairsFrom start

    in Just $ \i -> let (j, k) = workingSequence i in (fst $ kmono $ fst $ jepi j, k)
fiveLemmaMonocal _ _ _ _ _ _ _ _ _ = Nothing

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
fiveLemma j k l@(Effective lgs lms _ _) m n f g h i =
  Effective lgs lms
    (fiveLemmaMonocal j k l m n f g h i)
    (fiveLemmaEpical  j k l m n f g h i)
