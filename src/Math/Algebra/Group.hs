-- | A group, with the data of the group allowed to be separate from
-- the data of an element
module Math.Algebra.Group where

class Group a where
  type Element a = s | s -> a
  prod :: a -> Element a -> Element a -> Element a
  unit :: a -> Element a
  inv :: a -> Element a -> Element a

class (Group a) => Abelian a

-- TODO: This could be defined via the group presentation machinery.
data Z = Z

instance Group Z where
  type Element Z = Integer
  prod _ = (+)
  unit _ = 0
  inv _ = negate
