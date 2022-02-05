-- | A group, with the data of the group allowed to be separate from
-- the data of an element
module Math.Algebra.Group where

class Group a where
  type Element a = s | s -> a
  prod :: a -> Element a -> Element a -> Element a
  unit :: a -> Element a
  inv :: a -> Element a -> Element a

class (Group a) => Abelian a

class (Group a) => FiniteGroup a where
  elements :: a -> [Element a]

-- TODO: These could be defined via the group presentation machinery.
data Z = Z

instance Group Z where
  type Element Z = Integer
  prod _ = (+)
  unit _ = 0
  inv _ = negate

instance Abelian Z

-- likely always small
newtype Zmod = Zmod Int

instance Group Zmod where
  type Element Zmod = Int
  prod (Zmod n) x y = (x + y) `mod` n
  unit _ = 0
  inv (Zmod n) x = negate x `mod` n

instance Abelian Zmod

instance FiniteGroup Zmod where
  elements (Zmod n) = [0..n-1]
