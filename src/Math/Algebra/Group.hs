-- | A group, with the data of the group allowed to be separate from
-- the data of an element
module Math.Algebra.Group where

import Data.List (sortBy)
import Data.Ord (comparing)

class Group a where
  type Element a = s | s -> a
  prod :: a -> Element a -> Element a -> Element a
  unit :: a -> Element a
  inv :: a -> Element a -> Element a

  -- | Raise a group element to an integer power.
  power :: a -> Integer -> Element a -> Element a
  power a n x
    | n < 0 = positive (negate n) (inv a x)
    | otherwise = positive n x
    where
      positive 0 _ = unit a
      positive n x
        | even n = positive (n `quot` 2) (prod a x x)
        | otherwise = prod a x (positive (n - 1) x)

class (Group a) => Abelian a where
  -- | Scale an element by an integer.
  scale :: a -> Integer -> Element a -> Element a
  scale = power

-- | Canonicalise a sparse list of values in an abelian group. Terms are
-- sorted by key, repeated keys are combined, and unit-valued terms are
-- omitted.
normaliseGroupTerms ::
  (Abelian a, Eq (Element a), Ord key) =>
  a ->
  [(key, Element a)] ->
  [(key, Element a)]
normaliseGroupTerms group = go . sortBy (comparing fst)
  where
    go [] = []
    go ((key, value) : terms) =
      let (sameKey, rest) = span ((== key) . fst) terms
          total =
            foldl'
              (\accumulator (_, nextValue) -> prod group accumulator nextValue)
              value
              sameKey
       in if total == unit group
            then go rest
            else (key, total) : go rest

class (Group a) => FiniteGroup a where
  elements :: a -> [Element a]

-- TODO: These could be defined via the group presentation machinery.
data Z = Z

instance Group Z where
  type Element Z = Integer
  prod _ = (+)
  unit _ = 0
  inv _ = negate
  power _ = (*)

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
  power (Zmod modulus) exponent (ZmodElement x) =
    ZmodElement $
      fromInteger ((exponent * toInteger x) `mod` toInteger modulus)

instance Abelian Zmod

instance FiniteGroup Zmod where
  elements (Zmod n) = ZmodElement <$> [0 .. n - 1]
