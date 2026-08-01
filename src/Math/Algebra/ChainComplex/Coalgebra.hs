-- | A comonoid object in chain complexes, also known as a DG-coalgebra.
module Math.Algebra.ChainComplex.Coalgebra where

import Control.Category.Constrained (id, (.))
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Tensor
import Prelude hiding (id, (.))

class ChainComplex a => Coalgebra a where
  counitMor :: a -> Morphism a ()
  delMor :: a -> Morphism a (Tensor a a)

-- | A coalgebra with a chosen coaugmentation.
class Coalgebra a => CoaugmentedCoalgebra a where
  coaugmentationMor :: a -> Morphism () a

  -- | The diagonal projected onto the coaugmentation coideal in both
  -- factors. For a connected coalgebra this removes the two counital terms.
  reducedDelMor :: a -> Morphism a (Tensor a a)
  reducedDelMor a = tensorFunc a a projection projection . delMor a
    where
      projection = id - coaugmentationMor a . counitMor a
