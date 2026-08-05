-- | Reduced words in a free group, with adjacent powers compressed.
module Math.Algebra.Group.Free (
  FreeGroup (..),
  FreeWord,
  reducedWord,
  wordPowers,
  singletonWord,
  inverseWord,
  powerWord,
  bindWord,
  mapWord,
)
where

import Math.Algebra.Group

data FreeGroup a = FreeGroup

-- | A canonical free-group word.
--
-- We maintain the invariant that exponents are nonzero, and adjacent
-- generators are distinct.
newtype FreeWord a = FreeWord {wordPowers :: [(a, Integer)]}
  deriving (Eq, Ord, Show)

reducedWord :: Eq a => [(a, Integer)] -> FreeWord a
reducedWord = FreeWord . reverse . foldl' push []
  where
    push powers (_, 0) = powers
    push ((previous, previousPower) : powers) (generator, exponent)
      | previous == generator =
          case previousPower + exponent of
            0 -> powers
            total -> (generator, total) : powers
    push powers poweredGenerator = poweredGenerator : powers

singletonWord :: a -> FreeWord a
singletonWord generator = FreeWord [(generator, 1)]

instance Eq a => Semigroup (FreeWord a) where
  FreeWord left <> FreeWord right = reducedWord (left ++ right)

instance Eq a => Monoid (FreeWord a) where
  mempty = FreeWord []

inverseWord :: FreeWord a -> FreeWord a
inverseWord (FreeWord powers) =
  FreeWord (fmap (\(generator, exponent) -> (generator, negate exponent)) (reverse powers))

instance Eq a => Group (FreeGroup a) where
  type Element (FreeGroup a) = FreeWord a
  prod _ = (<>)
  unit _ = mempty
  inv _ = inverseWord

powerWord :: Eq a => Integer -> FreeWord a -> FreeWord a
powerWord exponent word
  | exponent < 0 = positive (negate exponent) (inverseWord word)
  | otherwise = positive exponent word
  where
    positive 0 _ = mempty
    positive n value
      | even n = positive (n `quot` 2) (value <> value)
      | otherwise = value <> positive (n - 1) value

bindWord :: Eq b => FreeWord a -> (a -> FreeWord b) -> FreeWord b
bindWord (FreeWord powers) substitute =
  foldMap
    (\(generator, exponent) -> powerWord exponent (substitute generator))
    powers

mapWord :: Eq b => (a -> b) -> FreeWord a -> FreeWord b
mapWord f (FreeWord powers) =
  reducedWord (fmap (\(generator, exponent) -> (f generator, exponent)) powers)
