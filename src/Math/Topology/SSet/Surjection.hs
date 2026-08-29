-- | Order-preserving surjections between finite ordinals.
module Math.Topology.SSet.Surjection (
  Surjection (..),
  strictlyIncreasing,
  surjections,
  isValidSurjection,
  surjectionCodomainDegree,
  surjectionValue,
  surjectionValues,
  surjectionFibreSizes,
  surjectionRepeatsAt,
  removeRepeats,
  insertRepeats,
  precomposeFace,
  precomposeDegeneracy,
)
where

import Math.Topology.SSet.NSimplex (choose)

-- | A monotone surjection @[q] ->> [n]@, encoded by the @n@ positions at
-- which its value increases. Thus @[1,3]@ in domain degree four denotes the
-- sequence @[0,0,1,1,2]@.
data Surjection = Surjection
  { surjectionDomainDegree :: !Int,
    surjectionTransitions :: [Int]
  }
  deriving (Eq, Ord)

instance Show Surjection where
  show (Surjection _ ts) = show ts

strictlyIncreasing :: Ord a => [a] -> Bool
strictlyIncreasing xs = and (zipWith (<) xs (drop 1 xs))

-- | The monotone surjections @[q] ->> [n]@.
surjections :: Int -> Int -> [Surjection]
surjections q n
  | q < 0 || n < 0 || q < n = []
  | otherwise = Surjection q <$> choose n [0 .. q - 1]

-- | Whether the stored transitions encode a monotone surjection.
isValidSurjection :: Surjection -> Bool
isValidSurjection (Surjection q ts) =
  q >= 0
    && strictlyIncreasing ts
    && all (\i -> i >= 0 && i < q) ts

-- | The degree of the codomain ordinal.
surjectionCodomainDegree :: Surjection -> Int
surjectionCodomainDegree = length . surjectionTransitions

-- | The sizes of the consecutive fibres.
surjectionFibreSizes :: Surjection -> [Int]
surjectionFibreSizes (Surjection q ts) =
  zipWith (-) (ts ++ [q]) (-1 : ts)

-- | Evaluate a surjection at one domain vertex.
surjectionValue :: Surjection -> Int -> Int
surjectionValue (Surjection _ ts) i =
  length (takeWhile (< i) ts)

-- | Evaluate a surjection on all its domain vertices.
surjectionValues :: Surjection -> [Int]
surjectionValues a =
  concat (zipWith replicate (surjectionFibreSizes a) [0 ..])

-- | Whether the surjection is constant on the edge @[i,i+1]@.
surjectionRepeatsAt :: Surjection -> Int -> Bool
surjectionRepeatsAt (Surjection q ts) i =
  i >= 0 && i < q && i `notElem` ts

-- | Remove repeated domain edges at the given valid ascending positions.
removeRepeats :: [Int] -> Surjection -> Surjection
removeRepeats rs (Surjection q ts) =
  Surjection (q - length rs) (reindex <$> ts)
  where
    reindex i = i - length (takeWhile (< i) rs)

-- | Insert repeated domain edges at the given valid ascending positions.
insertRepeats :: [Int] -> Surjection -> Surjection
insertRepeats rs (Surjection q ts) =
  Surjection q' (reindex <$> ts)
  where
    q' = q + length rs
    reindex i = foldl' skipRepeat i rs
    skipRepeat i r
      | r <= i = i + 1
      | otherwise = i

-- | Precompose with the coface map that skips vertex @i@, or Nothing
-- if the result is not surjective,
precomposeFace :: Int -> Surjection -> Maybe Surjection
precomposeFace i (Surjection q ts)
  | q <= 0 = error "precomposeFace: face of a vertex"
  | i < 0 || i > q = error "precomposeFace: invalid face index"
  | singletonFibre = Nothing
  | otherwise = Just (Surjection (q - 1) (shift <$> ts))
  where
    singletonFibre =
      (i == 0 || i - 1 `elem` ts)
        && (i == q || i `elem` ts)
    shift j
      | j < i = j
      | otherwise = j - 1

-- | Precompose with the codegeneracy map that repeats vertex @i@.
precomposeDegeneracy :: Int -> Surjection -> Surjection
precomposeDegeneracy i (Surjection q ts)
  | i < 0 || i > q = error "precomposeDegeneracy: invalid degeneracy index"
  | otherwise = Surjection (q + 1) (shift <$> ts)
  where
    shift j
      | j < i = j
      | otherwise = j + 1
