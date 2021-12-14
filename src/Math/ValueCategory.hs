-- | A value-level category with a type of objects and a type of
--   'loose' morphisms. An `Arrow` is a `LooseMorphism` together with
--   its domain and codomain.

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Math.ValueCategory where

class (Semigroup (Arrow ob)) => ValueCategory ob where
  type LooseMorphism ob = (mor :: *) | mor -> ob

  looseid  :: ob -> LooseMorphism ob
  -- domain :: Morphism ob -> ob
  -- codomain :: Morphism ob -> ob

data Arrow a = Arrow { domain :: a, mor :: LooseMorphism a, codomain :: a }
deriving instance (Show a, Show (LooseMorphism a)) => Show (Arrow a)

data CompArrows a = CompArrows a (LooseMorphism a) a (LooseMorphism a) a

vid :: ValueCategory ob => ob -> Arrow ob
vid a = Arrow a (looseid a) a

-- instance (Semigroup (LooseMorphism a)) => Semigroup (Arrow a) where
--   (Arrow d g _) <> (Arrow _ f c) = Arrow d (g <> f) c

data Square a = Square
  { squareTop :: LooseMorphism a,
    squareBottom :: LooseMorphism a
  }

instance (Semigroup (LooseMorphism a)) => Semigroup (Square a) where
  (Square t b) <> (Square t' b') = Square (t <> t') (b <> b')

instance (ValueCategory ob) => ValueCategory (Arrow ob) where
  type LooseMorphism (Arrow ob) = Square ob

  looseid (Arrow d f c) = Square (looseid d) (looseid c)
  -- domain = Arrow . squareDomain
  -- codomain = Arrow . squareCodomain

instance (Semigroup (Arrow ob)) => Semigroup (Arrow (Arrow ob)) where
  (Arrow d@(Arrow dd _ dc) (Square tg bg) (Arrow md _ mc)) <>
    (Arrow _ (Square tf bf) c@(Arrow cd _ cc))
    = Arrow d (Square tfg bfg ) c
      where Arrow _ tfg _ = Arrow dd tg md <> Arrow md tf cd
            Arrow _ bfg _ = Arrow dc bg mc <> Arrow mc bf cc
