{-# LANGUAGE UndecidableInstances #-}

-- | Internal hom of chain complexes
module Math.Algebra.ChainComplex.Hom where

import Control.Category.Constrained (id, (.))
import qualified Control.Category.Constrained as Constrained

import Prelude hiding (Bounded, id, (.))

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.Combination

data Hom a b = Hom a b

-- Looks weird, but I think this is right, for bounded b.
data HomBasis a b = HomBasis a b
  deriving (Eq)
  deriving (Show)

instance (FiniteType a, ChainComplex b) => ChainComplex (Hom a b) where
  type Basis (Hom a b) = HomBasis (Basis a) (Basis b)

  isBasis (Hom a b) (HomBasis s t) = isBasis a s && isBasis b t

  degree (Hom a b) (HomBasis s t) = degree b t - degree a s

  -- This is obviously far less efficient than having a function Combination -> Combination
  diff (Hom a b) = Morphism (-1) $ \(HomBasis s t) ->
    let n = degree (Hom a b) (HomBasis s t)
     in fmap (HomBasis s) (diff b `onBasis` t)
          + kozulRule (n + 1) (Combination $ fmap (\s' -> (diff a `onBasis` s' `coeffOf` s, HomBasis s' t)) (basis a (n + 1)))

instance (FiniteType a, FiniteType b, Bounded b) => FiniteType (Hom a b) where
  dim (Hom a b) n = sum $ (\y -> dim b y * dim a (y - n)) <$> amplitude b
  basis (Hom a b) n = do
    y <- amplitude b
    t <- basis b y
    s <- basis a (y - n)
    return $ HomBasis s t

-- Just degree 0 for now
homcontramap :: (FiniteType a, ChainComplex a', ChainComplex b) => a -> a' -> Morphism a a' -> Morphism (Hom a' b) (Hom a b)
homcontramap a a' m = Morphism 0 $ \(HomBasis s' t) ->
  let abasis = basis a (degree a' s')
   in Combination $ normalise $ fmap (\s -> (m `onBasis` s `coeffOf` s', HomBasis s t)) abasis

hommap :: Morphism b b' -> Morphism (Hom a b) (Hom a b')
hommap m = Morphism 0 $ \(HomBasis s t) -> HomBasis s <$> m `onBasis` t

instance Constrained.Functor (UMorphism Int) (UMorphism Int) (HomBasis a) where
  fmap m = Morphism 0 $ \(HomBasis s t) -> HomBasis s <$> m `onBasis` t

homcounit :: (Eq (Basis a)) => Morphism (Tensor (Hom a b) a) b
homcounit = Morphism 0 $ \(HomBasis s t, s') -> if s == s' then singleComb t else Combination []

homunit :: (Bounded b) => Morphism a (Hom b (Tensor a b))
homunit = Morphism 0 $ \s -> undefined

--TODO: definitely needs a sign of some kind here
internalFunc ::
  (ChainComplex a, ChainComplex b, ChainComplex a', ChainComplex b') =>
  Morphism (Tensor (Hom a b) (Hom a' b')) (Hom (Tensor a a') (Tensor b b'))
internalFunc = Morphism 0 $ \(HomBasis s t, HomBasis s' t') -> singleComb $ HomBasis (s, s') (t, t')
-- internalFunc = hommap undefined . homunit
--    where
--     -- fiddle = αi . (id ⊗ α) . (id ⊗ (s ⊗ id)) . (id ⊗ αi) . α
--     -- (⊗) = tensorFunc
--     -- α = tensorAssoc
--     -- αi = tensorAssocInv
--   -- prodMor (Product a b) = (prodMor a × prodMor b) . αi . (id × α) . (id × (s × id)) . (id × αi) . α


-- instance (FiniteType a, Bounded a, Bounded b, Eq (HomBasis a b)) => Bounded (Hom a b) where
--   amplitude (Hom a b) =
