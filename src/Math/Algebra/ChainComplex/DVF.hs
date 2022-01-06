{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Discrete Vector Field on a Chain Complex
-- Following as:ez-dvf
module Math.Algebra.ChainComplex.DVF where

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Reduction
import Prelude hiding (id, (.))
import Control.Category.Constrained

data Incidence = Pos | Neg

incidenceCoef :: Num p => Incidence -> p
incidenceCoef Pos = 1
incidenceCoef Neg = -1

data Status a
  = Source a Incidence
  | Target a Incidence
  | Critical
  deriving (Functor)

class ChainComplex a => DVF a where
  -- TODO: Name??
  vf :: a -> Basis a -> Status (Basis a)

isCritical :: DVF a => a -> Basis a -> Bool
isCritical a b
  | Critical <- vf a b = True
  | otherwise = False

newtype CriticalComplex a = CriticalComplex a

-- Could be done as a use of the perturbation lemma, but I think these
-- direct definitions might end up being more efficient
instance (DVF a, Eq (Basis a)) => ChainComplex (CriticalComplex a) where
  type Basis (CriticalComplex a) = Basis a
  isBasis (CriticalComplex a) = isCritical a
  degree (CriticalComplex a) b = degree a b
  diff (CriticalComplex a) = dK a (diff a)

proj :: DVF a => a -> Morphism a (CriticalComplex a)
proj a = Morphism 0 $ \b -> case vf a b of
  Critical -> Combination [(1, b)]
  _ -> Combination []

incl :: DVF a => a -> Morphism (CriticalComplex a) a
incl a = Morphism 0 (\x -> Combination [(1, x)])

-- Called d_V
nullDiff :: DVF a => a -> Morphism a a
nullDiff a = Morphism (-1) $ \b -> case vf a b of
  Target sigma i -> Combination [(incidenceCoef i, sigma)]
  _ -> Combination []

-- Called d_V
nullCodiff :: DVF a => a -> Morphism a a
nullCodiff a = Morphism 1 $ \b -> case vf a b of
  Source tau i -> Combination [(incidenceCoef i, tau)]
  _ -> Combination []

h :: (DVF a, Eq (Basis a)) => a -> Morphism a a -> Morphism a a
h a d = Morphism 1 $ \b -> case vf a b of
  Source tau i -> d'_vb - h a d `onComb` ((d `onComb` d'_vb) - d'_vb)
    where
      d'_vb = Combination [(incidenceCoef i, tau)]
  _ -> Combination []

f :: forall a. (DVF a, Eq (Basis a)) => a -> Morphism a a -> Morphism a (CriticalComplex a)
f a d = ((.) :: _) (proj a) ((id :: Morphism a a) - (d . h a d))

g :: (DVF a, Eq (Basis a)) => a -> Morphism a a -> Morphism (CriticalComplex a) a
g a d = (id - (h a d . d)) . incl a

dK :: (DVF a, Eq (Basis a)) => a -> Morphism a a -> Morphism (CriticalComplex a) (CriticalComplex a)
dK a d = proj a . (d - (d . h a d . d)) . incl a

dvfReduction :: (DVF a, Eq (Basis a)) => a -> Reduction a (CriticalComplex a)
dvfReduction a = Reduction (f a d) (g a d) (h a d)
  where d = diff a
