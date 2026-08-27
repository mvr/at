-- | Discrete Vector Field on a Chain Complex
-- Following as:ez-dvf
module Math.Algebra.ChainComplex.DVF where

import Control.Category.Constrained (id, (.))
import qualified Control.Category.Constrained as Constrained
import Prelude hiding (id, return, (.))

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.Combination

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
  deriving (Constrained.Functor (->) (->)) via (Constrained.Wrapped Status)

class ChainComplex a => DVF a where
  vf :: a -> Basis a -> Status (Basis a)

-- | A discrete vector field with finitely many critical cells in each degree.
class DVF a => FiniteCritical a where
  criticalBasis :: a -> Int -> [Basis a]
  default criticalBasis :: FiniteType a => a -> Int -> [Basis a]
  criticalBasis a n = filter (isCritical a) (basis a n)

isCritical :: DVF a => a -> Basis a -> Bool
isCritical a b
  | Critical <- vf a b = True
  | otherwise = False

newtype CriticalComplex a = CriticalComplex a

-- Could be done as a use of the perturbation lemma, but I think these
-- direct definitions might end up being more efficient
instance DVF a => ChainComplex (CriticalComplex a) where
  type Basis (CriticalComplex a) = Basis a
  isBasis (CriticalComplex a) s = isBasis a s && isCritical a s
  degree (CriticalComplex a) = degree a
  diff (CriticalComplex a) = dK a (diff a)

instance FiniteCritical a => FiniteType (CriticalComplex a) where
  basis (CriticalComplex a) = criticalBasis a

proj :: DVF a => a -> Morphism a (CriticalComplex a)
proj a = Morphism 0 $ \b -> case vf a b of
    Critical -> singleComb b
    _ -> zeroCombination

incl :: DVF a => a -> Morphism (CriticalComplex a) a
incl _ = basisMorphism id

-- Called d_V
nullDiff :: DVF a => a -> Morphism a a
nullDiff a = Morphism (-1) $ \b -> case vf a b of
  Target sigma i -> incidenceCoef i .* singleComb sigma
  _ -> zeroCombination

-- Called d_V'
nullCodiff :: DVF a => a -> Morphism a a
nullCodiff a = Morphism 1 $ \b -> case vf a b of
  Source tau i -> incidenceCoef i .* singleComb tau
  _ -> zeroCombination

hWith :: DVF a => a -> Morphism a a -> Morphism a a
hWith a d = homotopy
  where
    homotopy = Morphism 1 $ memoiseOrd $ \b -> case vf a b of
      Source tau i ->
        d'_vb - homotopy `onComb` ((d `onComb` d'_vb) - singleComb b)
        where
          d'_vb = incidenceCoef i .* singleComb tau
      _ -> zeroCombination

h :: DVF a => a -> Morphism a a -> Morphism a a
h = hWith

fWith :: DVF a => a -> Morphism a a -> Morphism a a -> Morphism a (CriticalComplex a)
fWith a d homotopy = proj a . (id - (d . homotopy))

gWith :: DVF a => a -> Morphism a a -> Morphism a a -> Morphism (CriticalComplex a) a
gWith a d homotopy = (id - (homotopy . d)) . incl a

dKWith :: DVF a => a -> Morphism a a -> Morphism a a -> Morphism (CriticalComplex a) (CriticalComplex a)
dKWith a d homotopy = proj a . (d - (d . homotopy . d)) . incl a

f :: DVF a => a -> Morphism a a -> Morphism a (CriticalComplex a)
f a d = fWith a d (hWith a d)

g :: DVF a => a -> Morphism a a -> Morphism (CriticalComplex a) a
g a d = gWith a d (hWith a d)

dK :: DVF a => a -> Morphism a a -> Morphism (CriticalComplex a) (CriticalComplex a)
dK a d = dKWith a d (hWith a d)

dvfReduction :: DVF a => a -> Reduction a (CriticalComplex a)
dvfReduction a = Reduction (fWith a d homotopy) (gWith a d homotopy) homotopy
  where
    d = memoiseMorphism (diff a)
    homotopy = hWith a d

dvfEquivalence :: DVF a => a -> Equivalence a (CriticalComplex a)
dvfEquivalence a = Equivalence a id a (dvfReduction a) (CriticalComplex a)

-- A reduction transfers a strict algebra to an A-infinity algebra in
-- general. These instances would require additional multiplicative
-- compatibility conditions on the vector field.
{-
instance (Algebra a, DVF a) => Algebra (CriticalComplex a) where
  muMor (CriticalComplex a) = proj a . muMor a . (incl a ⊗ incl a)
    where
      (⊗) = tensorFunc (CriticalComplex a) (CriticalComplex a)
  unitMor (CriticalComplex a) = proj a . unitMor a

instance (CommAlgebra a, DVF a) => CommAlgebra (CriticalComplex a)
-}
