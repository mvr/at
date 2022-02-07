{-# LANGUAGE UndecidableInstances #-}

-- | A bicomplex aka double complex, of free Z-modules. We use the
-- convention that the squares in the complex *anticommute*.
module Math.Algebra.Bicomplex where

import Control.Category.Constrained (id, join, (.))
import qualified Control.Category.Constrained as Constrained
import Math.Algebra.ChainComplex hiding (FiniteType)
import qualified Math.Algebra.ChainComplex as CC (FiniteType)
import Prelude hiding (id, return, (.))

class Eq (Bibasis a) => Bicomplex a where
  type Bibasis a = s | s -> a

  isBibasis :: a -> Bibasis a -> Bool
  isBibasis _ _ = True

  bidegree :: a -> Bibasis a -> (Int, Int)
  hdiff :: a -> Bimorphism a a -- degree (-1, 0)
  vdiff :: a -> Bimorphism a a -- degree (0, -1)

class Bicomplex a => FiniteType a where
  bidim :: a -> (Int, Int) -> Int
  bidim a i = length (bibasis a i)
  -- * `all isSimplex (basis n)`
  bibasis :: a -> (Int, Int) -> [Bibasis a]

data UBimorphism a b = Bimorphism
  { bimorphismDegree :: (Int, Int),
    onBibasis :: a -> Combination b
  }

type Bimorphism a b = UBimorphism (Bibasis a) (Bibasis b)

validBicomb :: Bicomplex a => a -> Combination (Bibasis a) -> Bool
validBicomb a (Combination bs) = and $ fmap (\(_, b) -> isBibasis a b) bs

instance Constrained.Semigroupoid UBimorphism where
  type Object UBimorphism a = Eq a

  (Bimorphism (h1,v1) f2) . (Bimorphism (h2,v2) f1) = Bimorphism (h1 + h2, v1 + v2) (join . fmap f2 . f1)

instance Constrained.Category UBimorphism where
  id = Bimorphism (0,0) (\x -> Combination [(1, x)])

instance Eq b => Num (UBimorphism a b) where
  fromInteger 0 = Bimorphism (0, 0) (const (Combination []))
  fromInteger _ = error "Morphism: fromInteger"

  (Bimorphism d1 f1) + (Bimorphism _ f2) = Bimorphism d1 (\x -> f1 x + f2 x)
  negate (Bimorphism d f) = Bimorphism d (negate . f)

  (*) = error "Morphism: (*)"
  abs = error "Morphism: abs"
  signum = error "Morphism: signum"

newtype Tot a = Tot a

newtype TotBasis a = TotBasis a
  deriving Eq
  deriving Show via a

instance (Bicomplex a) => ChainComplex (Tot a) where
  type Basis (Tot a) = TotBasis (Bibasis a)

  isBasis (Tot a) (TotBasis b) = isBibasis a b

  degree (Tot a) (TotBasis b) =
    let (p, q) = bidegree a b in p + q
  diff (Tot a) = Morphism (-1) $ \(TotBasis b) ->
    fmap TotBasis $ hdiff a `onBibasis` b + vdiff a `onBibasis` b

instance (Bicomplex a, FiniteType a) => CC.FiniteType (Tot a) where
  basis (Tot a) d = do
    vd <- [0 .. d]
    let hd = d - vd
    TotBasis <$> bibasis a (hd, vd)
