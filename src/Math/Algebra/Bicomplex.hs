-- | A bicomplex aka double complex, of free Z-modules. We use the
-- convention that the squares in the complex *anticommute*.
module Math.Algebra.Bicomplex where

import Control.Category.Constrained (id, (.))
import qualified Control.Category.Constrained as Constrained
import Math.Algebra.ChainComplex hiding (FiniteType)
import qualified Math.Algebra.ChainComplex as CC (FiniteType)
import Math.Algebra.Combination
import Prelude hiding (id, return, (.))

newtype Bidegree = Bidegree (Int, Int)
  deriving (Eq, Show) via (Int, Int)

-- This should just be a monoid instance.
instance Num Bidegree where
  fromInteger 0 = Bidegree (0, 0)
  fromInteger _ = error "Bidegree: fromInteger"

  (Bidegree (h, v)) + (Bidegree (h', v')) = Bidegree (h + h', v + v')
  negate _ = error "Bidegree: negate"

  (*) = error "Bidegree: (*)"
  abs = error "Bidegree: abs"
  signum = error "Bidegree: signum"

class Ord (Bibasis a) => Bicomplex a where
  type Bibasis a

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

  -- | The finite list of bidegrees to inspect when totalising in a given
  -- degree. The default covers first-quadrant bicomplexes.
  totalBidegrees :: a -> Int -> [(Int, Int)]
  totalBidegrees _ d
    | d < 0 = []
    | otherwise = [(d - vd, vd) | vd <- [0 .. d]]

data Bimorphism a b = Bimorphism
  { bimorphismDegree :: Bidegree,
    onBibasis :: Bibasis a -> Combination (Bibasis b)
  }

-- | Retag a bimorphism with unchanged endpoint bibasis types.
sameBibasisMorphism ::
  (Bibasis a ~ Bibasis a', Bibasis b ~ Bibasis b') =>
  Bimorphism a b ->
  Bimorphism a' b'
sameBibasisMorphism (Bimorphism d f) = Bimorphism d f

-- | Regard a vertical-degree-preserving morphism as horizontal.
horizontaliseMorphism ::
  (Basis a ~ Bibasis a', Basis b ~ Bibasis b') =>
  Morphism a b ->
  Bimorphism a' b'
horizontaliseMorphism (Morphism d f) =
  Bimorphism (Bidegree (d, 0)) f

-- | Regard a horizontal-degree-preserving morphism as vertical.
verticaliseMorphism ::
  (Basis a ~ Bibasis a', Basis b ~ Bibasis b') =>
  Morphism a b ->
  Bimorphism a' b'
verticaliseMorphism (Morphism d f) =
  Bimorphism (Bidegree (0, d)) f

bimorphismZeroOfDeg :: Bidegree -> Bimorphism a b
bimorphismZeroOfDeg d = Bimorphism d (const zeroCombination)

instance Constrained.Semigroupoid Bimorphism where
  type Object Bimorphism a = Bicomplex a
  (Bimorphism d2 f2) . (Bimorphism d1 f1) =
    Bimorphism (d1 + d2) (\basisElement -> bindCombination (f1 basisElement) f2)

instance Constrained.Category Bimorphism where
  id = Bimorphism (Bidegree (0, 0)) singleComb

instance (Bicomplex a, Bicomplex b) => Num (Bimorphism a b) where
  fromInteger 0 = bimorphismZeroOfDeg (Bidegree (0, 0))
  fromInteger _ = error "Bimorphism: fromInteger"
  Bimorphism d f + Bimorphism _ g = Bimorphism d (\x -> f x + g x)
  negate (Bimorphism d f) = Bimorphism d (negate . f)
  (*) = error "Bimorphism: (*)"
  abs = error "Bimorphism: abs"
  signum = error "Bimorphism: signum"

validBicomb :: Bicomplex a => a -> Combination (Bibasis a) -> Bool
validBicomb a combination = and $ fmap (\(_, b) -> isBibasis a b) (coeffs combination)

newtype Tot a = Tot a

-- | Totalise a bimorphism by summing its bidegree.
totaliseMorphism ::
  (Bicomplex a, Bicomplex b) =>
  Bimorphism a b ->
  Morphism (Tot a) (Tot b)
totaliseMorphism (Bimorphism (Bidegree (h, v)) f) =
  Morphism (h + v) f

instance (Bicomplex a) => ChainComplex (Tot a) where
  type Basis (Tot a) = Bibasis a

  isBasis (Tot a) = isBibasis a

  degree (Tot a) b =
    let (p, q) = bidegree a b in p + q
  diff (Tot a) =
    totaliseMorphism (hdiff a) + totaliseMorphism (vdiff a)

instance (Bicomplex a, FiniteType a) => CC.FiniteType (Tot a) where
  basis (Tot a) d = do
    bideg <- totalBidegrees a d
    bibasis a bideg
