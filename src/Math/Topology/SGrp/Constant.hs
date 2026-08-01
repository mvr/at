{-# LANGUAGE UndecidableInstances #-}

-- | The constant simplicial group associated to an ordinary group.
module Math.Topology.SGrp.Constant where

import Math.Algebra.Group
import Math.Topology.SGrp
import Math.Topology.SSet

newtype ConstantGroup g = ConstantGroup g

instance Show g => Show (ConstantGroup g) where
  show (ConstantGroup g) = "ConstantGroup(" ++ show g ++ ")"

newtype ConstantGroupSimplex g = ConstantGroupSimplex {constantGroupValue :: Element g}

deriving instance Eq (Element g) => Eq (ConstantGroupSimplex g)
deriving instance Ord (Element g) => Ord (ConstantGroupSimplex g)
deriving instance Show (Element g) => Show (ConstantGroupSimplex g)

instance (Group g, Ord (Element g)) => SSet (ConstantGroup g) where
  type GeomSimplex (ConstantGroup g) = ConstantGroupSimplex g

  geomSimplexDim _ _ = 0
  geomFace _ _ _ = undefined

instance (Group g, Ord (Element g)) => Pointed (ConstantGroup g) where
  basepoint (ConstantGroup g) = ConstantGroupSimplex (unit g)

instance (FiniteGroup g, Ord (Element g)) => FiniteType (ConstantGroup g) where
  geomBasis (ConstantGroup g) 0 = ConstantGroupSimplex <$> elements g
  geomBasis _ _ = []

instance (Group g, Ord (Element g)) => SGrp (ConstantGroup g) where
  prodMor (ConstantGroup g) = Morphism $ \(left, right) ->
    NonDegen $
      ConstantGroupSimplex $
        prod
          g
          (constantGroupValue $ underlyingGeom left)
          (constantGroupValue $ underlyingGeom right)

  invMor (ConstantGroup g) = Morphism $ \(ConstantGroupSimplex value) ->
    NonDegen $ ConstantGroupSimplex $ inv g value

instance (Abelian g, Ord (Element g)) => SAb (ConstantGroup g)
