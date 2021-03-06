{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Math.ValueCategory where

class ValueCategory ob where
  type Morphism ob = (mor :: *) | mor -> ob

  vid  :: ob -> Morphism ob
  domain :: Morphism ob -> ob
  codomain :: Morphism ob -> ob

  (.*) :: Morphism ob -> Morphism ob -> Morphism ob
