{-# LANGUAGE UndecidableInstances #-}

-- | A bicomplex aka double complex, of free Z-modules. We use the
-- convention that the squares in the complex *anticommute*.
module Math.Algebra.Bicomplex where

import Math.Algebra.ChainComplex

class Bicomplex a where
  type Bibasis a = s | s -> a

  bidegree :: a -> Bibasis a -> (Int, Int)
  hdiff :: a -> Bimorphism a a -- degree (-1, 0)
  vdiff :: a -> Bimorphism a a -- degree (0, -1)

data UBimorphism a b = Bimorphism
  { bimorphismDegree :: (Int, Int),
    onBibasis :: a -> Combination b
  }

type Bimorphism a b = UBimorphism (Bibasis a) (Bibasis b)

newtype Tot a = Tot a

newtype TotBasis a = TotBasis a

instance (Bicomplex a, Eq (Bibasis a)) => ChainComplex (Tot a) where
  type Basis (Tot a) = TotBasis (Bibasis a)

  degree (Tot a) (TotBasis b) =
    let (p, q) = bidegree a b in p + q
  diff (Tot a) = Morphism (-1) $ \(TotBasis b) ->
    fmap TotBasis $ hdiff a `onBibasis` b + vdiff a `onBibasis` b
