-- Temporarily
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Additive Category
module Math.ValueCategory.Additive where

import Math.ValueCategory

class (Num (LooseMorphism a), ValueCategory a) => AdditiveCategory a where
  zero :: a
  looseToZero :: a -> LooseMorphism a
  looseToZero a = looseZeroMorphism a zero

  looseFromZero :: a -> LooseMorphism a
  looseFromZero b = looseZeroMorphism zero b

  looseZeroMorphism :: a -> a -> LooseMorphism a

-- looseZeroMorphism a b = looseFromZero b <> looseToZero a

-- looseAddMorphisms :: LooseMorphism a -> LooseMorphism a -> LooseMorphism a
-- looseSubtractMorphisms :: LooseMorphism a -> LooseMorphism a -> LooseMorphism a
-- looseNegateMorphism :: LooseMorphism a -> LooseMorphism a

-- {-# MINIMAL
--  zero,
--  (toZero, fromZero | zeroMorphism),
--  addMorphisms,
--  (subtractMorphisms | negateMorphism) #-}

toZero :: AdditiveCategory a => a -> Arrow a
toZero a = Arrow a (looseToZero a) zero

fromZero :: AdditiveCategory a => a -> Arrow a
fromZero a = Arrow zero (looseFromZero a) a

zeroArrow :: AdditiveCategory a => a -> a -> Arrow a
zeroArrow a b = Arrow a (looseZeroMorphism a b) b

instance AdditiveCategory a => Num (Arrow a) where
  (Arrow a f b) + (Arrow _ g _) = Arrow a (f + g) b
  negate (Arrow a f b) = Arrow a (negate f) b
