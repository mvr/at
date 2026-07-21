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

newtype Zmod = Zmod Int

newtype ZmodElement = ZmodElement Int
  deriving (Eq, Ord, Show, Num)

zmodElement :: (Integral a) => Zmod -> a -> ZmodElement
zmodElement (Zmod n) x = ZmodElement (fromIntegral x `mod` n)

instance Group Zmod where
  type Element Zmod = ZmodElement
  prod group (ZmodElement x) (ZmodElement y) = zmodElement group (x + y)
  unit _ = ZmodElement 0
  inv group (ZmodElement x) = zmodElement group (negate x)

instance Abelian Zmod

instance FiniteGroup Zmod where
  elements (Zmod n) = ZmodElement <$> [0 .. n - 1]
