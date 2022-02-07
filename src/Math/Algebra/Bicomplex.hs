{-# LANGUAGE UndecidableInstances #-}

-- | A bicomplex aka double complex, of free Z-modules. We use the
-- convention that the squares in the complex *anticommute*.
module Math.Algebra.Bicomplex where

import Math.Algebra.ChainComplex hiding (FiniteType)
import qualified Math.Algebra.ChainComplex as CC (FiniteType)
import Prelude hiding (id, return, (.))

newtype Bidegree = Bidegree (Int, Int)
  deriving Show via (Int, Int)

-- This should just be a monoid instance.
instance Num Bidegree where
  fromInteger 0 = Bidegree (0, 0)
  fromInteger _ = error "Bidegree: fromInteger"

  (Bidegree (h,v)) + (Bidegree (h',v')) = Bidegree (h+h', v+v')
  negate _ = error "Bidegree: negate"

  (*) = error "Bidegree: (*)"
  abs = error "Bidegree: abs"
  signum = error "Bidegree: signum"

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

type Bimorphism a b = UMorphism Bidegree (Bibasis a) (Bibasis b)

validBicomb :: Bicomplex a => a -> Combination (Bibasis a) -> Bool
validBicomb a (Combination bs) = and $ fmap (\(_, b) -> isBibasis a b) bs

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
    fmap TotBasis $ hdiff a `onBasis` b + vdiff a `onBasis` b

instance (Bicomplex a, FiniteType a) => CC.FiniteType (Tot a) where
  basis (Tot a) d = do
    vd <- [0 .. d]
    let hd = d - vd
    TotBasis <$> bibasis a (hd, vd)
