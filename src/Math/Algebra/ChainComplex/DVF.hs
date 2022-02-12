{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Discrete Vector Field on a Chain Complex
-- Following as:ez-dvf
module Math.Algebra.ChainComplex.DVF where

import Data.Coerce
import Math.Algebra.Combination
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Equivalence
import Prelude hiding (id, (.), return)
import Control.Category.Constrained (id, (.), return)

-- Units of Z
data Incidence = Pos | Neg

incidenceCoef :: Num p => Incidence -> p
incidenceCoef Pos = 1
incidenceCoef Neg = -1

flipIncidence :: Incidence -> Incidence
flipIncidence Pos = Neg
flipIncidence Neg = Pos

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
newtype CriticalBasis a = CriticalBasis a
  deriving (Eq, Show)

-- Could be done as a use of the perturbation lemma, but I think these
-- direct definitions might end up being more efficient
instance DVF a => ChainComplex (CriticalComplex a) where
  type Basis (CriticalComplex a) = CriticalBasis (Basis a)
  isBasis (CriticalComplex a) (CriticalBasis s) = isBasis a s && isCritical a s
  degree (CriticalComplex a) (CriticalBasis b) = degree a b
  diff (CriticalComplex a) = dK a (diff a)

instance (DVF a, FiniteType a) => FiniteType (CriticalComplex a) where
  basis (CriticalComplex a) n = coerce $ filter (isCritical a) (basis a n)

proj :: DVF a => a -> Morphism a (CriticalComplex a)
proj a = Morphism 0 $ coerce $ \b -> case vf a b of
  Critical -> return b
  _ -> Combination []

incl :: DVF a => a -> Morphism (CriticalComplex a) a
incl a = fmapBasis coerce

-- Called d_V
nullDiff :: DVF a => a -> Morphism a a
nullDiff a = Morphism (-1) $ \b -> case vf a b of
  Target sigma i -> Combination [(incidenceCoef i, sigma)]
  _ -> Combination []

-- Called d_V'
nullCodiff :: DVF a => a -> Morphism a a
nullCodiff a = Morphism 1 $ \b -> case vf a b of
  Source tau i -> Combination [(incidenceCoef i, tau)]
  _ -> Combination []

h :: (DVF a, Eq (Basis a)) => a -> Morphism a a -> Morphism a a
h a d = Morphism 1 $ \b -> case vf a b of
  Source tau i -> d'_vb - h a d `onComb` ((d `onComb` d'_vb) - singleComb b)
    where
      d'_vb = Combination [(incidenceCoef i, tau)]
  _ -> Combination []

f :: forall a. DVF a => a -> Morphism a a -> Morphism a (CriticalComplex a)
f a d = proj a . (id - (d . h a d))

g :: DVF a => a -> Morphism a a -> Morphism (CriticalComplex a) a
g a d = (id - (h a d . d)) . incl a

dK :: DVF a => a -> Morphism a a -> Morphism (CriticalComplex a) (CriticalComplex a)
dK a d = proj a . (d - (d . h a d . d)) . incl a

dvfReduction :: DVF a => a -> Reduction a (CriticalComplex a)
dvfReduction a = Reduction (f a d) (g a d) (h a d)
  where d = diff a

dvfEquivalence :: DVF a => a -> Equivalence a (CriticalComplex a)
dvfEquivalence a = Equivalence a id a (dvfReduction a) (CriticalComplex a)

-- When does this actually work?
instance (Algebra a, DVF a) => Algebra (CriticalComplex a) where
  muMor (CriticalComplex a) = proj a . muMor a . (incl a `t` incl a)
    where t = tensorMorphism (CriticalComplex a) (CriticalComplex a)
  unitMor (CriticalComplex a) = proj a . unitMor a
