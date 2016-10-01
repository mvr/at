{-# LANGUAGE RecordWildCards #-}
module Math.Algebra.FGAbelianGroup where

import Data.List (intercalate, group, sort)
import Data.Matrix as M
import qualified Data.Vector as V
import Math.Algebra.SmithNormalForm

--------------------------------------------------------------------------------

data IsoClass = IsoClass
  {
    rank    :: Integer,
    torsion :: [Integer] -- Should be sorted prime powers
  } deriving Eq

instance Show IsoClass where
  show IsoClass{..} =
    if rank > 0 || length torsion > 0 then
      intercalate " + " $ (replicate (fromIntegral rank) "Z") ++
                          fmap (\ps -> if length ps == 1 then
                                         "Z/" ++ show (head ps) ++ "Z"
                                       else
                                         "(Z/" ++ show (head ps) ++ "Z)^" ++ show (length ps)
                                   ) (group torsion)
    else
      "0"

factors :: Integer -> [Integer]
factors = f (head primes) (tail primes) where
  f n ns m
    | m < 2          = []
    | m < n^2        = [m]
    | m `mod` n == 0 = n : f n ns (m `div` n)
    | otherwise      = f (head ns) (tail ns) m

primes :: [Integer]
primes = 2 : filter (\n -> head (factors n) == n) [3,5..]

splitFactors :: Integer -> [Integer]
splitFactors = fmap product . group . factors

fromInvariantFactors :: Integer -> [Integer] -> IsoClass
fromInvariantFactors rank factors =
  IsoClass rank (sort $ factors >>= splitFactors)

--------------------------------------------------------------------------------

data FGAbelianGroup = FGAbelianGroup
  {
    presentation :: Matrix Integer,
    reduced      :: Matrix Integer,

    toReduced    :: Matrix Integer,
    fromReduced  :: Matrix Integer
  }

instance Eq FGAbelianGroup where
  a == b = reduced a == reduced b

isoClass :: FGAbelianGroup -> IsoClass
isoClass (FGAbelianGroup _ d _ _) = fromInvariantFactors (fromIntegral r) diag
  where diag = filter (/= 0) $ V.toList $ getDiag d
        r    = M.nrows d - length diag

instance Show FGAbelianGroup where
  show = show . isoClass

-- Direct sum
instance Monoid FGAbelianGroup where
  -- TODO

zeroGroup :: FGAbelianGroup
zeroGroup = FGAbelianGroup (M.fromList 1 1 [1]) (M.fromList 1 1 [1]) (M.fromList 1 1 [1]) (M.fromList 1 1 [1])

-- Remove useless generators
-- Delete rows and cols with a 1 on the diagonal
stripOnes :: (Matrix Integer, Matrix Integer, Matrix Integer)
          -> (Matrix Integer, Matrix Integer, Matrix Integer)
stripOnes (li, l, d) = (li,
                        M.submatrix 1       (M.nrows l) newcols (M.ncols l) l,
                        M.submatrix newrows (M.nrows d) newcols (M.ncols d) d)
  where diag = V.toList $ M.getDiag d
        countOnes = length $ takeWhile (==1) diag
        -- If the diagonal is all 1s, we have to be careful to avoid an empty matrix
        (newrows, newcols) = case (countOnes == M.nrows d, countOnes == M.ncols d) of
                               (True, True) -> (countOnes, countOnes)
                               (True, False) -> (countOnes, countOnes)
                               (False, True) -> (countOnes + 1, countOnes)
                               (False, False) -> (countOnes + 1, countOnes + 1)

-- Remove useless relations
-- Delete cols that are all 0
-- TODO: this may fail on d with 0 cols
stripZeroes :: (Matrix Integer, Matrix Integer, Matrix Integer)
            -> (Matrix Integer, Matrix Integer, Matrix Integer)
stripZeroes (li, l, d) = (li,
                          l,
                          M.submatrix 1 (M.nrows d) 1 nonZeroes d
                         )
  where diag = V.toList $ M.getDiag d
        nonZeroes = max (length $ filter (/=0) diag) 1

reducePresentation :: Matrix Integer -> (Matrix Integer, Matrix Integer, Matrix Integer)
reducePresentation m = let (Triple li l d _ _) = smithNormalForm m
                           (li', l', d') = stripOnes (li, l, d)
                       in stripZeroes (li', l', d')

fromPresentation :: Matrix Integer -> FGAbelianGroup
fromPresentation m = FGAbelianGroup m d li l
  where (li, l, d) = reducePresentation m

--------------------------------------------------------------------------------
-- General integer matrix routines
-- (Do they need their own file?)

-- Z is "coherent": Given M, we compute a L such that
-- MX = 0   <->   exists Y. X = LY
-- So the image of L is the kernel of M
matrixKernel :: Matrix Integer -> Matrix Integer
matrixKernel m = if nonzeroes /= 0 then
                   M.forceMatrix $ M.submatrix 1 (M.nrows ri) (nonzeroes + 1) (M.ncols ri) ri
                 else
                   M.identity (M.ncols m)
  where (Triple _ _ d _ ri) = smithNormalForm m
        diag   = V.toList $ M.getDiag d
        nonzeroes = length $ takeWhile (/=0) diag

matrixKernelModulo :: Matrix Integer -> Matrix Integer -> Matrix Integer
matrixKernelModulo m l = M.forceMatrix $ M.submatrix 1 (M.ncols m) 1 (M.ncols bigl) bigl
  where bigl = matrixKernel (m <|> l)

--------------------------------------------------------------------------------

data Morphism = Morphism {
  domain :: FGAbelianGroup,
  codomain :: FGAbelianGroup,
  fullMorphism :: Matrix Integer,
  reducedMorphism :: Matrix Integer
}

morphismFromFullMatrix :: FGAbelianGroup -> FGAbelianGroup -> Matrix Integer -> Morphism
morphismFromFullMatrix a b f = Morphism a b f (toReduced b * f * fromReduced a)

morphismFromReducedMatrix :: FGAbelianGroup -> FGAbelianGroup -> Matrix Integer -> Morphism
morphismFromReducedMatrix a b f = Morphism a b (fromReduced b * f * toReduced a ) f

identityMorphism :: FGAbelianGroup -> Morphism
identityMorphism a = Morphism a a (M.identity $ M.nrows $ presentation a) (M.identity $ M.nrows $ reduced a)

zeroMorphism :: FGAbelianGroup -> FGAbelianGroup -> Morphism
zeroMorphism a b = morphismFromFullMatrix a b (M.zero (M.nrows $ presentation b) (M.nrows $ presentation a))

--------------------------------------------------------------------------------

kernel :: Morphism -> FGAbelianGroup
kernel f = fromPresentation ker
  where ker   = matrixKernelModulo kappa            (presentation (domain f))
        kappa = matrixKernelModulo (fullMorphism f) (presentation (codomain f))

cokernel :: Morphism -> FGAbelianGroup
cokernel f = fromPresentation (fullMorphism f <|> presentation (codomain f))
