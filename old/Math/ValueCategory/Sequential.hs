{-# LANGUAGE TypeFamilies #-}
module Math.ValueCategory.Sequential where

import Math.ValueCategory

data Sequential a = Sequential {
  sequenceObs :: Integer -> a,
  sequenceMors :: Integer -> Integer -> Morphism a
}

data SequentialMorphism a = SequentialMorphism {
  sequentialDomain :: Sequential a,
  sequentialCodomain :: Sequential a,
  pointwiseMorphisms :: Integer -> Morphism a
}

instance (ValueCategory ob) => ValueCategory (Sequential ob) where
  type Morphism (Sequential ob) = SequentialMorphism ob

  vid s@(Sequential obs mors) = SequentialMorphism s s (\i -> vid $ sequenceObs s i)
  domain = sequentialDomain
  codomain = sequentialCodomain

  (SequentialMorphism _ c ms) .* (SequentialMorphism d _ ms') = SequentialMorphism d c (\i -> ms i .* ms' i)

constantSequence :: ValueCategory a => a -> Sequential a
constantSequence g = Sequential (const g) (const $ const $ vid g)

autocomposeMorphisms :: ValueCategory a => (Integer -> Morphism a) -> (Integer -> Integer -> Morphism a)
autocomposeMorphisms ms i j
  | i == j     = vid $ domain $ ms i
  | j == i + 1 = ms i
  | otherwise  = autocomposeMorphisms ms (i+1) j .* ms i
