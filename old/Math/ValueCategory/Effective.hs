{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Math.ValueCategory.Effective where

import Math.ValueCategory
import Math.ValueCategory.Abelian
import Math.ValueCategory.Sequential

--------------------------------------------------------------------------------
-- Following
-- "Effective Algebraic Topology"
--  by Rolf Schön
-- Mem. Amer. Math. Soc. 92 (1991), no. 451, vi+63 pp.

-- TODO: Consider intmap memo instead
data Effective a = Effective
  { effectiveSequence :: Sequential a,
    monocal :: Maybe (Integer -> (Integer, Integer)),
    epical :: Maybe (Integer -> (Integer, Integer))
  }

pattern Monocal gs ms mono <-
  Effective (Sequential gs ms) (Just mono) _
  where
    Monocal gs ms mono = Effective (Sequential gs ms) (Just mono) Nothing

pattern Epical gs ms epi <-
  Effective (Sequential gs ms) _ (Just epi)
  where
    Epical gs ms epi = Effective (Sequential gs ms) Nothing (Just epi)

pattern Isocal gs ms iso <-
  ((\i@(Effective (Sequential gs ms) mono epi) -> (gs, ms, isocal i)) -> (gs, ms, Just iso))
  where
    Isocal gs ms iso = Effective (Sequential gs ms) (Just iso) (Just iso)

isocalFunc ::
  (Integer -> (Integer, Integer)) ->
  (Integer -> (Integer, Integer)) ->
  (Integer -> (Integer, Integer))
isocalFunc mono epi i = let j = fst $ epi i in mono j

isocal :: Effective a -> Maybe (Integer -> (Integer, Integer))
isocal (Effective _ mmono mepi) = do
  mono <- mmono
  epi <- mepi
  return (isocalFunc mono epi)

instance (Show a, AbelianCategory a) => Show (Effective a) where
  show (Isocal _ ms iso) = show $ imageObject $ ms i j
    where
      (i, j) = iso 1
  show (Epical _ _ _) = "<An epical group>"
  show (Monocal _ _ _) = "<A monocal group>"
  show _ = "<A totally unknown group>"

constantEffectiveGroup :: (ValueCategory a) => a -> Effective a
constantEffectiveGroup g = Effective (constantSequence g) (Just idIndices) (Just idIndices)
  where
    idIndices i = (i, i)

--------------------------------------------------------------------------------
-- Morphisms

data EffectiveMorphism a = EffectiveMorphism
  { effectiveDomain :: Effective a,
    effectiveCodomain :: Effective a,
    sequenceMorphism :: SequentialMorphism a
  }

instance (ValueCategory a) => ValueCategory (Effective a) where
  type Morphism (Effective a) = EffectiveMorphism a

  vid e = EffectiveMorphism e e (vid $ effectiveSequence e)

  domain = effectiveDomain
  codomain = effectiveCodomain

  (EffectiveMorphism _ c f') .* (EffectiveMorphism _ d f) =
    EffectiveMorphism d c (f' .* f)

--------------------------------------------------------------------------------
-- The Five Lemma Algorithm

-- The setup is an sequence of Effectives whose colimits fit into an
-- exact sequence:
-- J -f-> K -g-> L -h-> M -i-> N

-- We are trying to determine epi/monocals for L. Here we do not
-- require the levelwise sequences to be exact, only the
-- colimiting sequence.

-- TODO: A different ordering may be more efficient
enumeratePairs :: [(Integer, Integer)]
enumeratePairs = [(m - n, n) | m <- [0 ..], n <- [0 .. m]]

enumeratePairsFrom :: (Integer -> Integer) -> [(Integer, Integer)]
enumeratePairsFrom start = do
  m <- [0 ..]
  let mStart = start m
  n <- [mStart .. (m + mStart)]
  return (m - n, n)

-- enumeratePairsFrom :: (Integer, Integer) -> [(Integer, Integer)]
-- enumeratePairsFrom (a, b) = fmap (\(i, j) -> (i + a, j + b)) enumeratePairs

fiveLemmaEpical ::
  (Eq a, AbelianCategory a) =>
  Effective a ->
  Effective a ->
  Effective a ->
  Effective a ->
  Effective a ->
  EffectiveMorphism a ->
  EffectiveMorphism a ->
  EffectiveMorphism a ->
  EffectiveMorphism a ->
  Maybe (Integer -> (Integer, Integer))
fiveLemmaEpical
  _
  (Epical kgs kms kepi)
  (Effective (Sequential lgs lms) _ _)
  (Epical mgs mms mepi)
  (Monocal ngs nms nmono)
  _
  (EffectiveMorphism _ _ (SequentialMorphism _ _ g))
  (EffectiveMorphism _ _ (SequentialMorphism _ _ h))
  (EffectiveMorphism _ _ (SequentialMorphism _ _ i)) =
    let start j = maximum [kl, ml, nl]
          where
            (ke, kl) = kepi j
            (me, ml) = mepi ke
            (ne, nl) = nmono me

        -- K(ke(j), k) -> L(ke(j), k) -> M(me(ke(j)), k) -> N(ne(me(ke(j))), k)
        toComplex (j, k) =
          [ imageMorphism (kms ke k) (lms ke k) (g k),
            imageMorphism (lms ke k) (mms me k) (h k),
            imageMorphism (mms me k) (nms ne k) (i k)
          ]
          where
            (ke, kl) = kepi j
            (me, ml) = mepi ke
            (ne, nl) = nmono me

        workingSequence i = head $ filter (isExactSequence . toComplex) $ enumeratePairsFrom start
     in Just $ \i -> let (j, k) = workingSequence i in (fst $ kepi j, k)
fiveLemmaEpical _ _ _ _ _ _ _ _ _ = Nothing

fiveLemmaMonocal ::
  (Eq a, AbelianCategory a) =>
  Effective a ->
  Effective a ->
  Effective a ->
  Effective a ->
  Effective a ->
  EffectiveMorphism a ->
  EffectiveMorphism a ->
  EffectiveMorphism a ->
  EffectiveMorphism a ->
  Maybe (Integer -> (Integer, Integer))
fiveLemmaMonocal
  (Epical jgs jms jepi)
  (Monocal kgs kms kmono)
  (Effective (Sequential lgs lms) _ _)
  (Monocal mgs mms mmono)
  _
  (EffectiveMorphism _ _ (SequentialMorphism _ _ f))
  (EffectiveMorphism _ _ (SequentialMorphism _ _ g))
  (EffectiveMorphism _ _ (SequentialMorphism _ _ h))
  _ =
    let start j = maximum [jl, kl, ml]
          where
            (je, jl) = jepi j
            (ke, kl) = kmono je
            (me, ml) = mmono ke

        toComplex (j, k) =
          [ imageMorphism (jms je k) (kms ke k) (f k),
            imageMorphism (kms ke k) (lms ke k) (g k),
            imageMorphism (lms ke k) (mms me k) (h k)
          ]
          where
            (je, jl) = jepi j
            (ke, kl) = kmono je
            (me, ml) = mmono ke

        workingSequence i = head $ filter (isExactSequence . toComplex) $ enumeratePairsFrom start
     in Just $ \i -> let (j, k) = workingSequence i in (fst $ kmono $ fst $ jepi j, k)
fiveLemmaMonocal _ _ _ _ _ _ _ _ _ = Nothing

fiveLemma ::
  (Eq a, AbelianCategory a) =>
  Effective a ->
  Effective a ->
  Effective a ->
  Effective a ->
  Effective a ->
  EffectiveMorphism a ->
  EffectiveMorphism a ->
  EffectiveMorphism a ->
  EffectiveMorphism a ->
  Effective a
fiveLemma j k l@(Effective (Sequential lgs lms) _ _) m n f g h i =
  Effective
    (Sequential lgs lms)
    (fiveLemmaMonocal j k l m n f g h i)
    (fiveLemmaEpical j k l m n f g h i)

--------------------------------------------------------------------------------
-- The Five Lemma Algorithm for levelwise exact sequences
