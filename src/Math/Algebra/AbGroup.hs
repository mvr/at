{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Math.Algebra.AbGroup where

import qualified Data.Matrix as M
import Data.Matrix (Matrix, (<|>))
import qualified Data.Vector as V
import Math.Algebra.SmithNormalForm
import Math.Algebra.AbGroup.IsoClass
import Math.ValueCategory
import Math.ValueCategory.Abelian

data AbGroup = AbGroup
  {
    presentation :: Matrix Integer,
    reduced      :: Matrix Integer,

    toReduced    :: Matrix Integer,
    fromReduced  :: Matrix Integer
  }

instance Eq AbGroup where
  a == b = reduced a == reduced b

isoClass :: AbGroup -> IsoClass
isoClass (AbGroup _ d _ _) = IsoClass (fromIntegral r) (invariantFactorsToElementaryDivisors diag)
  where diag = filter (/= 0) $ V.toList $ M.getDiag d
        r    = M.nrows d - length diag

instance Show AbGroup where
  show = show . isoClass

fromIsoClass :: IsoClass -> AbGroup
fromIsoClass (IsoClass 0 []) = zero
fromIsoClass (IsoClass rank torsion) = AbGroup m m i i
  where invFactors = elementaryDivisorsToInvariantFactors torsion
        rows = fromIntegral (rank + fromIntegral (length invFactors))
        cols = max 1 (length invFactors)
        m = M.extendTo 0 rows cols $
            M.diagonal 0 $ V.fromList invFactors
        i = M.identity rows

freeAbGroup :: Integer -> AbGroup
freeAbGroup n = fromIsoClass (IsoClass n [])

-- Direct sum
instance Monoid AbGroup where
  -- TODO

-- Remove useless generators:
-- Delete rows and cols with a 1 on the diagonal
stripOnes :: (Matrix Integer, Matrix Integer, Matrix Integer)
          -> (Matrix Integer, Matrix Integer, Matrix Integer)
stripOnes (li, l, d) = (M.submatrix newrows (M.nrows li) 1      (M.ncols li) li,
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

-- Remove useless relations:
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

fromPresentation :: Matrix Integer -> AbGroup
fromPresentation m = AbGroup m d li l
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

-- This solves (M L) (X Y)^T = 0 and returns the part of the solution
-- corresponding to X.
matrixKernelModulo :: Matrix Integer -> Matrix Integer -> Matrix Integer
matrixKernelModulo m l = M.forceMatrix $ M.submatrix 1 (M.ncols m) 1 (M.ncols bigl) bigl
  where bigl = matrixKernel (m <|> l)

--------------------------------------------------------------------------------

data AbMorphism = AbMorphism {
  abDomain :: AbGroup,
  abCodomain :: AbGroup,
  fullMorphism :: Matrix Integer,
  reducedMorphism :: Matrix Integer
} deriving (Show, Eq)

morphismFromFullMatrix :: AbGroup -> AbGroup -> Matrix Integer -> AbMorphism
morphismFromFullMatrix a b f = AbMorphism a b f (toReduced b * f * fromReduced a)

morphismFromReducedMatrix :: AbGroup -> AbGroup -> Matrix Integer -> AbMorphism
morphismFromReducedMatrix a b f = AbMorphism a b (fromReduced b * f * toReduced a) f

instance ValueCategory AbGroup where
  type Morphism AbGroup = AbMorphism

  vid a  = AbMorphism a a (M.identity $ M.nrows $ presentation a) (M.identity $ M.nrows $ reduced a)

  domain = abDomain
  codomain = abCodomain

  (AbMorphism _ c f' r') .* (AbMorphism d _ f r) = AbMorphism d c (f' * f) (r' * r)

--------------------------------------------------------------------------------
-- Following some ideas in
-- "A Coq Formalization of Finitely Presented Modules"
--  by Cyril Cohen and Anders Mörtberg
-- International Conference on Interactive Theorem Proving
-- Springer International Publishing, 2014

-- TODO: Check performance vs. using reduced matrices.
instance AbelianCategory AbGroup where
  zero = AbGroup (M.fromList 1 1 [1])
                 (M.fromList 1 1 [1])
                 (M.fromList 1 1 [1])
                 (M.fromList 1 1 [1])

  zeroMorphism a b = morphismFromFullMatrix a b (M.zero (M.nrows $ presentation b) (M.nrows $ presentation a))

  kernelObject f = fromPresentation ker
    where ker   = matrixKernelModulo kappa            (presentation (domain f))
          kappa = matrixKernelModulo (fullMorphism f) (presentation (codomain f))

  kernel f = morphismFromFullMatrix (fromPresentation ker) (domain f) kappa
    where ker   = matrixKernelModulo kappa            (presentation (domain f))
          kappa = matrixKernelModulo (fullMorphism f) (presentation (codomain f))

  kernelMorphism f g phi = undefined

  cokernelObject f = fromPresentation (fullMorphism f <|> presentation (codomain f))

  -- TODO: just supply reduced matrix directly
  cokernel f = morphismFromFullMatrix (codomain f) (cokernelObject f)
               (M.identity $ M.nrows $ presentation $ codomain f)

  cokernelMorphism f g phi = morphismFromFullMatrix (cokernelObject f) (cokernelObject g) (fullMorphism phi)
