{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Math.ValueCategory.Abelian.Cached where

import Math.ValueCategory
import Math.ValueCategory.Abelian

data Cached a = Cached {getCached :: a, cachedIsZero :: Bool}

data CachedMorphism a
  = CachedZeroMorphism
      { cachedDomain :: Cached a,
        cachedCodomain :: Cached a
      }
  | CachedIdentityMorphism
      { cachedDomain :: Cached a
      }
  | CachedMorphism
      { cachedDomain :: Cached a,
        cachedCodomain :: Cached a,
        cachedMorphism :: Morphism a,
        cachedKernel :: CachedMorphism a,
        cachedCokernel :: CachedMorphism a
      }

instance Show a => Show (Cached a) where
  show = show . getCached

instance (AbelianCategory a, Eq a, Eq (Morphism a), Show (Morphism a)) => Show (CachedMorphism a) where
  show (CachedZeroMorphism d c) = show (zeroMorphism d c)
  show (CachedIdentityMorphism d) = show (vid d)
  show f = show (cachedMorphism f)

instance (Eq a) => Eq (Cached a) where
  (Cached _ True) == (Cached _ True) = True
  (Cached a False) == (Cached b False) = a == b
  _ == _ = False

instance (Eq a) => Eq (CachedMorphism a) where
  (==) = undefined -- TODO

-- instance Monoid (CachedMorphism a) where
--   (CachedMorphism )

instance (Eq a, Eq (Morphism a), AbelianCategory a) => ValueCategory (Cached a) where
  type Morphism (Cached a) = CachedMorphism a

  vid c = CachedIdentityMorphism c

  domain = cachedDomain
  codomain (CachedIdentityMorphism d) = d
  codomain f = cachedCodomain f

  (CachedIdentityMorphism _) .* f = f
  f .* (CachedIdentityMorphism _) = f
  (CachedZeroMorphism _ c) .* f = zeroMorphism (domain f) c
  f .* (CachedZeroMorphism d _) = zeroMorphism d (codomain f)
  f .* g = toCachedMorphism (cachedMorphism f .* cachedMorphism g)

instance (Eq a, Eq (Morphism a), AbelianCategory a) => AbelianCategory (Cached a) where
  zero = Cached zero True
  zeroMorphism a b = CachedZeroMorphism a b

  kernel (CachedZeroMorphism d c) = CachedIdentityMorphism d
  kernel (CachedIdentityMorphism c) = fromZero c
  kernel f = cachedKernel f

  kernelMorphism f g phi = toCachedMorphism $ kernelMorphism (cachedMorphism f) (cachedMorphism g) (cachedMorphism phi) -- TODO

  cokernel (CachedZeroMorphism d c) = CachedIdentityMorphism c
  cokernel (CachedIdentityMorphism c) = toZero c
  cokernel f = cachedCokernel f
  cokernelMorphism f g phi = toCachedMorphism $ cokernelMorphism (cachedMorphism f) (cachedMorphism g) (cachedMorphism phi) -- TODO

toCached :: (Eq a, AbelianCategory a) => a -> Cached a
toCached a =
  Cached
    { getCached = a,
      cachedIsZero = a == zero
    }

toCachedMorphism :: (Eq a, Eq (Morphism a), AbelianCategory a) => Morphism a -> CachedMorphism a
toCachedMorphism f | isZeroMorphism f = CachedZeroMorphism (toCached $ domain f) (toCached $ codomain f)
toCachedMorphism f | f == vid (domain f) = CachedIdentityMorphism (toCached $ domain f)
toCachedMorphism f =
  CachedMorphism
    { cachedDomain = toCached (domain f),
      cachedCodomain = toCached (codomain f),
      cachedMorphism = f,
      cachedKernel = toCachedMorphism $ kernel f,
      cachedCokernel = toCachedMorphism $ cokernel f
    }
