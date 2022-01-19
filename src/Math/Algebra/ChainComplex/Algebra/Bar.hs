{-# LANGUAGE UndecidableInstances #-}

-- | The bar construction of a DG-algebra `A`, specifically,
-- `Bar(Z,A,Z)`.  The bar construction of an ordinary algebra is a
-- special case (sometimes called the 'standard complex').
--
-- There are lots of places that the signs can go wrong. Here are
-- various descriptions of the bar construction.
-- Section 2.2.2 in http://math.uchicago.edu/~may/REU2019/REUPapers/Zhang,Ruoqi(Rachel).pdf
-- Also https://ncatlab.org/nlab/show/bar+and+cobar+construction
-- Also Homology, MacLane, Chapter X.10 (Kenzo claims there is a sign error)
--
-- To reduce the surface area of where sign issues can creep in, I am
-- going to factor the construction into the composite of simpler
-- ones:
-- Alg(Z) -Bar-> {Simplicial objects in Ch(Z)} -Dold-Kan-> biCh(Z) -Tot-> Ch(Z)
--
-- Whatever we do should end up compatible with the sign choices made
-- by Kenzo, so we can confirm things are going right.
--
-- TODO: compare sign choices with
-- https://www-users.cse.umn.edu/~tlawson/papers/signs.pdf
-- Not promising: "This brings us to a dear friend whose sign
-- conventions have personally given me nightmares on more than one
-- occasion. Namely, the bar construction — or specifically, in this
-- case, the bar construction of a differential graded algebra with
-- coefficients in a pair of differential graded modules. "
module Math.Algebra.ChainComplex.Algebra.Bar where

import Data.Coerce
import Math.Algebra.Bicomplex
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra

newtype Bar a = Bar a

newtype BarBibasis a = BarBibasis a
  deriving (Eq)

newtype BarBasis a = BarBasis a
  deriving (Eq)

instance (Algebra a, Eq (Basis a)) => Bicomplex (Bar a) where
  type Bibasis (Bar a) = BarBibasis [Basis a]
  bidegree (Bar a) (BarBibasis bs) = (length bs, sum (degree a <$> bs))

  -- TODO: Catastrophic sign issues in here,
  -- will need to fix via many unit tests

  vdiff (Bar a) = Bimorphism (0, -1) (coerce go)
    where
      go :: [Basis a] -> Combination [Basis a]
      go [] = 0
      go (b : bs) = fmap (: bs) (diff a `onBasis` b) + kozulRule (degree a b) (go bs)

  hdiff (Bar a) = Bimorphism (-1, 0) (coerce go)
    where
      go :: [Basis a] -> Combination [Basis a]
      go [] = 0
      go [b1] = 0
      go (b1 : b2 : bs) = fmap (: bs) (muMor a `onBasis` (b1, b2)) + kozulRule (degree a b1) (go (b2 : bs))

-- Can this be done using DerivingVia?
instance (Algebra a, Eq (Basis a)) => ChainComplex (Bar a) where
  type Basis (Bar a) = BarBasis (Basis (Tot (Bar a))) -- Reuse the same carrier Bar
  degree (Bar a) = coerce (degree (Tot (Bar a)))
  diff (Bar a) = coerce (diff (Tot (Bar a)))

-- TODO: universal twisting cochain a -> Bar a (should be same as the one induced by the twist on Wbar)
