{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Math.ValueCategory.Abelian.Cached where

import Math.ValueCategory
import Math.ValueCategory.Abelian

data Cached a = Cached { getCached :: a, cachedIsZero :: Bool }

data CachedMorphism a = CachedMorphism {
  cachedDomain :: Cached a,
  cachedCodomain :: Cached a,
  cachedMorphism :: Morphism a,
  cachedIsZeroMorphism :: Bool,
  cachedIsIdentity :: Bool,
  cachedKernel :: CachedMorphism a,
  cachedCokernel :: CachedMorphism a
}

pattern CachedZeroMorphism :: (Eq a, Eq (Morphism a), AbelianCategory a) => Cached a -> Cached a -> CachedMorphism a
pattern CachedZeroMorphism a b <- CachedMorphism a b _ True _ _ _ where
  CachedZeroMorphism a b = CachedMorphism a b (zeroMorphism (getCached a) (getCached b)) True (a == zero && b == zero) (vid a) (vid b)

pattern CachedIdentityMorphism :: (Eq a, Eq (Morphism a), AbelianCategory a) => Cached a -> CachedMorphism a
pattern CachedIdentityMorphism a <- CachedMorphism a a _ _ True _ _ where
  CachedIdentityMorphism c@(Cached a isZero) = CachedMorphism c c (vid a) isZero True (fromZero c) (toZero c)

instance (Eq a) => Eq (Cached a) where
  (Cached _ True) == (Cached _ True) = True
  (Cached a False) == (Cached b False) = a == b
  _ == _ = False

-- instance Monoid (CachedMorphism a) where
--   (CachedMorphism )

instance (Eq a, Eq (Morphism a), AbelianCategory a) => ValueCategory (Cached a) where
  type Morphism (Cached a) = CachedMorphism a

  vid c@(Cached a isZero) = CachedMorphism c c (vid a) isZero True (fromZero c) (toZero c)

  domain   = cachedDomain
  codomain = cachedCodomain

  (CachedIdentityMorphism _) .* f  = f
  f .* (CachedIdentityMorphism _) = f
  (CachedZeroMorphism _ c)       .* (CachedMorphism d _ _ _ _ _ _) = zeroMorphism d c
  (CachedMorphism _ c _ _ _ _ _) .* (CachedZeroMorphism d _)       = zeroMorphism d c
  (CachedMorphism _ _ g _ _ _ _) .* (CachedMorphism _ _ f _ _ _ _) = toCachedMorphism (g .* f)


instance (Eq a, Eq (Morphism a), AbelianCategory a) => AbelianCategory (Cached a) where
  zero = Cached zero True
  toZero a = CachedMorphism a zero (toZero $ getCached a) True (a == zero) (vid a) (vid zero)
  fromZero b = CachedMorphism zero b (fromZero $ getCached b) True (b == zero) (vid zero) (vid b)
  zeroMorphism a b = CachedMorphism a b (zeroMorphism (getCached a) (getCached b)) True (a == zero && b == zero) (vid a) (vid b)

  kernel = cachedKernel
  kernelMorphism f g phi = toCachedMorphism $ kernelMorphism (cachedMorphism f) (cachedMorphism g) (cachedMorphism phi) -- TODO

  cokernel = cachedCokernel
  cokernelMorphism f g phi = toCachedMorphism $ cokernelMorphism (cachedMorphism f) (cachedMorphism g) (cachedMorphism phi) -- TODO

toCached :: (Eq a, AbelianCategory a) => a -> Cached a
toCached a = Cached {
               getCached = a,
               cachedIsZero = a == zero
             }

toCachedKernel :: (Eq a, Eq (Morphism a), AbelianCategory a) => Morphism a -> CachedMorphism a
toCachedKernel f = CachedMorphism {
  cachedDomain         = kObject,
  cachedCodomain       = toCached $ domain f,
  cachedMorphism       = k,
  cachedIsZeroMorphism = kObject == zero,
  cachedIsIdentity     = isZeroMorphism f,
  cachedKernel         = fromZero $ kObject,
  cachedCokernel       = toCachedCokernel k
}
  where k = kernel f
        kObject = toCached $ domain k

toCachedCokernel :: (Eq a, Eq (Morphism a), AbelianCategory a) => Morphism a -> CachedMorphism a
toCachedCokernel f = CachedMorphism {
  cachedDomain         = toCached $ codomain f,
  cachedCodomain       = ckObject,
  cachedMorphism       = ck,
  cachedIsZeroMorphism = ckObject == zero,
  cachedIsIdentity     = isZeroMorphism f,
  cachedKernel         = toCachedKernel ck,
  cachedCokernel       = toZero $ ckObject
}
  where ck = cokernel f
        ckObject = toCached $ codomain ck

toCachedMorphism :: (Eq a, Eq (Morphism a), AbelianCategory a) => Morphism a -> CachedMorphism a
toCachedMorphism f = CachedMorphism {
  cachedDomain = d,
  cachedCodomain = c,
  cachedMorphism = f,
  cachedIsZeroMorphism = isZero,
  cachedIsIdentity = isIdentity,
  cachedKernel = k,
  cachedCokernel = ck
}
  where d = toCached $ domain f
        c = toCached $ codomain f
        isZero = isZeroMorphism f
        isIdentity = f == vid (domain f)
        k = case (isZero, isIdentity) of
              (True, _) -> vid d
              (_, True) -> fromZero d
              (_, _)    -> toCachedKernel f
        ck = case (isZero, isIdentity) of
              (True, _) -> vid c
              (_, True) -> toZero c
              (_, _)    -> toCachedCokernel f
