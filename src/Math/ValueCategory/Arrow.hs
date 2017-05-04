{-# LANGUAGE TypeFamilies #-}
module Math.ValueCategory.Arrow where

import Math.ValueCategory

newtype Arrow a = Arrow (Morphism a)

data Square a = Square { squareDomain :: Morphism a,
                         squareCodomain :: Morphism a,
                         squareTop :: Morphism a,
                         squareBottom :: Morphism a }

instance (ValueCategory ob) => ValueCategory (Arrow ob) where
  type Morphism (Arrow ob) = Square ob

  vid (Arrow f) = Square f f (vid $ domain f) (vid $ codomain f)
  domain = Arrow . squareDomain
  codomain = Arrow . squareCodomain

  (Square _ c t b) .* (Square d _ t' b') = Square d c (t .* t') (b .* b')
