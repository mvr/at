-- | The Cobar construction of a one-reduced coaugmented DG-coalgebra.
--
-- The underlying graded module is the tensor algebra
-- \(T(s^{-1}\bar C)\). A word stores basis elements of \(C\), while its
-- degree accounts for one desuspension per letter. Degree-zero basis
-- elements are omitted because they span the chosen coaugmentation.
module Math.Algebra.ChainComplex.Coalgebra.Cobar where

import Math.Algebra.Bicomplex hiding (FiniteType)
import qualified Math.Algebra.Bicomplex as Bi (FiniteType (..))
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Coalgebra
import Math.Algebra.Combination

-- | The tensor algebra on the desuspension of a chain complex. No
-- connectedness or reducedness is required: every basis element of the
-- supplied complex is available as a tensor generator. The general
-- construction deliberately has no 'FiniteType' instance: a degree-one
-- generator desuspends to degree zero, allowing arbitrarily long words in a
-- fixed degree.
newtype TensorDesusp c = TensorDesusp c

-- | The coaugmentation coideal of a one-reduced complex. In this case it is
-- represented by the basis elements in degrees at least two.
newtype CoaugmentationCoideal c = CoaugmentationCoideal c

-- | The tensor algebra on the desuspended coaugmentation coideal.
type CobarTensor c = TensorDesusp (CoaugmentationCoideal c)

-- | The unperturbed tensor algebra underlying the Cobar construction.
cobarTensor :: c -> CobarTensor c
cobarTensor = TensorDesusp . CoaugmentationCoideal

-- | The Cobar construction, including the differential induced by the
-- reduced coproduct.
newtype Cobar c = Cobar c

-- Homological desuspension uses d(s^-1 c) = -s^-1(dc). Extend this to
-- tensor words as a derivation.
internalWordDiff :: ChainComplex c => c -> [Basis c] -> Combination [Basis c]
internalWordDiff _ [] = 0
internalWordDiff c (b : bs) =
  -mapCombination (: bs) (diff c `onBasis` b)
    + kozulRule
      (degree c b - 1)
      (mapCombination (b :) (internalWordDiff c bs))

instance ChainComplex c => Bicomplex (TensorDesusp c) where
  type Bibasis (TensorDesusp c) = [Basis c]

  isBibasis (TensorDesusp c) = all (isBasis c)
  bidegree (TensorDesusp c) bs =
    (-length bs, sum (degree c <$> bs))
  hdiff _ = bimorphismZeroOfDeg (Bidegree (-1, 0))
  vdiff (TensorDesusp c) =
    Bimorphism (Bidegree (0, -1)) (internalWordDiff c)

instance ChainComplex c => ChainComplex (TensorDesusp c) where
  type Basis (TensorDesusp c) = [Basis c]

  isBasis (TensorDesusp c) = isBasis (Tot (TensorDesusp c))
  degree (TensorDesusp c) = degree (Tot (TensorDesusp c))
  diff (TensorDesusp c) =
    sameBasisMorphism (diff (Tot (TensorDesusp c)))

instance OneReducedChainComplex c => ChainComplex (CoaugmentationCoideal c) where
  type Basis (CoaugmentationCoideal c) = Basis c

  isBasis (CoaugmentationCoideal c) b =
    degree c b >= 2 && isBasis c b
  degree (CoaugmentationCoideal c) = degree c
  diff (CoaugmentationCoideal c) =
    Morphism (-1) (onBasis (diff c))

instance
  (OneReducedChainComplex c, FiniteType c) =>
  FiniteType (CoaugmentationCoideal c)
  where
  basis (CoaugmentationCoideal c) d
    | d < 2 = []
    | otherwise = filter (isBasis (CoaugmentationCoideal c)) (basis c d)

coidealWords ::
  (OneReducedChainComplex c, FiniteType c) =>
  CoaugmentationCoideal c ->
  Int ->
  Int ->
  [[Basis c]]
coidealWords _ 0 0 = [[]]
coidealWords _ _ wordLength | wordLength <= 0 = []
coidealWords _ totalDegree wordLength
  | totalDegree < 2 * wordLength = []
coidealWords coideal totalDegree wordLength = do
  generatorDegree <- [2 .. totalDegree - 2 * (wordLength - 1)]
  b <- basis coideal generatorDegree
  bs <- coidealWords coideal (totalDegree - generatorDegree) (wordLength - 1)
  pure (b : bs)

secondQuadrantBidegrees :: Int -> [(Int, Int)]
secondQuadrantBidegrees totalDegree
  | totalDegree < 0 = []
  | otherwise =
      [(-wordLength, totalDegree + wordLength) | wordLength <- [0 .. totalDegree]]

instance
  (OneReducedChainComplex c, FiniteType c) =>
  Bi.FiniteType (TensorDesusp (CoaugmentationCoideal c))
  where
  bibasis (TensorDesusp coideal) (horizontalDegree, verticalDegree)
    | horizontalDegree > 0 = []
    | otherwise =
        coidealWords coideal verticalDegree (-horizontalDegree)

  totalBidegrees _ = secondQuadrantBidegrees

instance
  (OneReducedChainComplex c, FiniteType c) =>
  FiniteType (TensorDesusp (CoaugmentationCoideal c))
  where
  basis tensorDesusp = basis (Tot tensorDesusp)

instance
  OneReducedChainComplex c =>
  ConnectedChainComplex (TensorDesusp (CoaugmentationCoideal c))

instance ChainComplex c => Algebra (TensorDesusp c) where
  unitMor _ = basisMorphism (const [])
  muMor _ = basisMorphism (\(left, right) -> left ++ right)

instance ChainComplex c => AugmentedAlgebra (TensorDesusp c) where
  augmentationMor _ = Morphism 0 $ \word ->
    if null word then singleComb () else zeroCombination

-- The sign on a generator is
--
-- d_delta(s^-1 c) = sum (-1)^|c'| (s^-1 c' | s^-1 c''),
--
-- where the sum ranges over the terms of the reduced coproduct. Extend this
-- to tensor words as a derivation.
coproductWordDiff ::
  CoaugmentedCoalgebra c =>
  c ->
  [Basis c] ->
  Combination [Basis c]
coproductWordDiff _ [] = 0
coproductWordDiff c (b : bs) =
  mapCombination (++ bs) (generatorDiff b)
    + kozulRule
      (degree c b - 1)
      (mapCombination (b :) (coproductWordDiff c bs))
  where
    generatorDiff generator =
      bindCombination (reducedDelMor c `onBasis` generator) $ \(left, right) ->
        kozulRule (degree c left) (singleComb [left, right])

instance
  (CoaugmentedCoalgebra c, OneReducedChainComplex c) =>
  Bicomplex (Cobar c)
  where
  type Bibasis (Cobar c) = [Basis c]

  isBibasis (Cobar c) = isBibasis (cobarTensor c)
  bidegree (Cobar c) = bidegree (cobarTensor c)
  vdiff (Cobar c) =
    Bimorphism
      (Bidegree (0, -1))
      (onBibasis (vdiff (cobarTensor c)))
  hdiff (Cobar c) =
    Bimorphism (Bidegree (-1, 0)) (coproductWordDiff c)

instance
  (CoaugmentedCoalgebra c, OneReducedChainComplex c, FiniteType c) =>
  Bi.FiniteType (Cobar c)
  where
  bibasis (Cobar c) = bibasis (cobarTensor c)
  totalBidegrees _ = secondQuadrantBidegrees

instance
  (CoaugmentedCoalgebra c, OneReducedChainComplex c) =>
  ChainComplex (Cobar c)
  where
  type Basis (Cobar c) = [Basis c]

  isBasis (Cobar c) = isBasis (Tot (Cobar c))
  degree (Cobar c) = degree (Tot (Cobar c))
  diff (Cobar c) = sameBasisMorphism (diff (Tot (Cobar c)))

instance
  (CoaugmentedCoalgebra c, OneReducedChainComplex c) =>
  ConnectedChainComplex (Cobar c)

instance
  (CoaugmentedCoalgebra c, OneReducedChainComplex c, FiniteType c) =>
  FiniteType (Cobar c)
  where
  basis (Cobar c) = basis (cobarTensor c)

instance
  (CoaugmentedCoalgebra c, OneReducedChainComplex c) =>
  Algebra (Cobar c)
  where
  unitMor _ = basisMorphism (const [])
  muMor _ = basisMorphism (\(left, right) -> left ++ right)

instance
  (CoaugmentedCoalgebra c, OneReducedChainComplex c) =>
  AugmentedAlgebra (Cobar c)
  where
  augmentationMor _ = Morphism 0 $ \word ->
    if null word then singleComb () else zeroCombination

-- | The horizontal part of the Cobar differential, expressed as a
-- perturbation of the tensor algebra on the desuspended coaugmentation
-- coideal.
cobarPerturbation ::
  (CoaugmentedCoalgebra c, OneReducedChainComplex c) =>
  c ->
  Morphism (CobarTensor c) (CobarTensor c)
cobarPerturbation c = Morphism (-1) (coproductWordDiff c)
