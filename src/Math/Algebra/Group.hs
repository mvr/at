-- | A group, with the data of the group allowed to be separate from
-- the data of an element

module Math.Algebra.Group where

class Group a where
  type Element a = s | s -> a
  prod :: a -> Element a -> Element a -> Element a
  unit :: a -> Element a
  inv  :: a -> Element a -> Element a

class (Group a) => Abelian a where
