module Math.Algebra.SmithNormalForm (
  Triple(..),
  smithNormalForm
) where

import Data.Monoid (All(..))
import Data.List (foldl')
import Control.Monad.State

import Data.Matrix (Matrix)
import qualified Data.Matrix as M


-- This entire file could benefit from being more lensy
-- And more recursive, although if we are looking for speed
-- it should just be rewritten in C.

--------------------------------------------------------------------------------
-- Utilities missing from Data.Matrix

combineCols :: Num a => Int -> a -> Int -> Matrix a -> Matrix a
combineCols c1 l c2 m = M.mapCol (\j x -> x + l * M.getElem j c2 m) c1 m

-- scaleCol :: Num a => a -> Int -> Matrix a -> Matrix a
-- scaleCol = M.mapCol . const . (*)

foldWithIndices :: [(Int, Int)] -> ((Int, Int) -> a -> b -> b) -> b -> Matrix a -> b
foldWithIndices is f s m = foldl' (\b (i, j) -> f (i, j) (M.getElem i j m) b) s is

foldWithIndex :: ((Int, Int) -> a -> b -> b) -> b -> Matrix a -> b
foldWithIndex f s m = foldWithIndices [ (i, j) | j <- [1 .. M.ncols m],  i <- [1 .. M.nrows m] ] f s m

--------------------------------------------------------------------------------
-- Triples
-- These keep track of a (L, M, R) decomposition of a matrix

data Triple = Triple { leftInverse :: Matrix Integer,
                       left   :: Matrix Integer,
                       middle :: Matrix Integer,
                       right  :: Matrix Integer,
                       rightInverse :: Matrix Integer}
  deriving (Show, Eq)

matrixToTriple :: Matrix Integer -> Triple
matrixToTriple m = Triple (M.identity $ M.nrows m) (M.identity $ M.nrows m) m (M.identity $ M.ncols m) (M.identity $ M.ncols m)

swapRows :: Int -> Int -> Triple -> Triple
swapRows i j (Triple li l m r ri) = Triple (M.switchRows i j li) (M.switchCols i j l) (M.switchRows i j m) r ri

swapCols :: Int -> Int -> Triple -> Triple
swapCols i j (Triple li l m r ri) = Triple li l (M.switchCols i j m) (M.switchRows i j r) (M.switchCols i j ri)

addRowMultiple :: Int -> Integer -> Int -> Triple -> Triple
addRowMultiple i x j (Triple li l m r ri) = Triple (M.combineRows i x j li) (combineCols j (-x) i l) (M.combineRows i x j m) r ri

addColMultiple :: Int -> Integer -> Int -> Triple -> Triple
addColMultiple i x j (Triple li l m r ri) = Triple li l (combineCols i x j m) (M.combineRows j (-x) i r) (combineCols i x j ri)

negateRow :: Int -> Triple -> Triple
negateRow i (Triple li l m r ri) = Triple (M.mapRow (const negate) i li) (M.mapCol (const negate) i l) (M.mapRow (const negate) i m) r ri

--------------------------------------------------------------------------------
-- Smith Normal Form
-- (There are almost certainly more efficient algorithms)

lowerRightBlock :: Int -> Matrix a -> Matrix a
lowerRightBlock s m = M.submatrix s r s c m
  where r = M.nrows m
        c = M.ncols m

findSmallest :: (Num a, Ord a) => Matrix a -> (a, (Int, Int))
findSmallest m = foldWithIndex f (M.getElem 1 1 m, (1, 1)) m
  where f newIndices new (smallest, smallestIndices) =
          if (smallest == 0) || (new /= 0 && abs new < abs smallest) then (new, newIndices)
          else (smallest, smallestIndices)

findSmallestRemainder :: Integer -> Matrix Integer -> (Integer, (Int, Int))
findSmallestRemainder x = foldWithIndex f (x, (1, 1))
  where f newIndices new (smallest, smallestIndices) =
          let r = new `rem` x in
          if r /= 0 && r < smallest then (new, newIndices)
          else (smallest, smallestIndices)

edgingIndices :: Int -> Matrix a -> [(Int, Int)]
edgingIndices s m = [ (i, s) | i <- [s+1 .. M.nrows m]] ++ [ (s, j) | j <- [s+1 .. M.ncols m]]

findSmallestInEdging :: (Num a, Ord a) => Int -> Matrix a -> (a, (Int, Int))
findSmallestInEdging s m = foldWithIndices (edgingIndices s m) f (M.getElem s s m, (s, s)) m
  where f newIndices new (smallest, smallestIndices) =
          if new /= 0 && abs new < abs smallest then (new, newIndices)
          else (smallest, smallestIndices)

moveLeastToStart :: Int -> State Triple ()
moveLeastToStart s = do
  t <- get
  let (smallest, (blockR, blockC)) = findSmallest $ lowerRightBlock s $ middle t
      smallestR = s + blockR - 1
      smallestC = s + blockC - 1
  when (smallestR /= s) $ modify $ swapRows smallestR s
  when (smallestC /= s) $ modify $ swapCols smallestC s
  when (smallest < 0)   $ modify $ negateRow s

modifyEdging :: Int -> State Triple ()
modifyEdging s = do
  t <- get
  let m = middle t
      mss = M.getElem s s m

  forM_ [s+1 .. M.nrows m] $ \i ->
    when (M.getElem i s m /= 0) $ modify $
      let q = M.getElem i s m `quot` mss in
      addRowMultiple i (-q) s

  forM_ [s+1 .. M.ncols m] $ \j ->
    when (M.getElem s j m /= 0) $ modify $
      let q = M.getElem s j m `quot` mss in
      addColMultiple j (-q) s


moveLeastEdgingToStart :: Int -> State Triple ()
moveLeastEdgingToStart s = do
  t <- get
  let (_smallest, (smallestR, smallestC)) = findSmallestInEdging s $ middle t
  when (smallestR /= s) $ modify $ swapRows smallestR s
  when (smallestC /= s) $ modify $ swapCols smallestC s

-- TODO: short circut
matrixIsNull :: Matrix Integer -> Bool
matrixIsNull m = getAll $ foldWithIndex (\_ i a -> a <> All (i == 0)) (All True) m

edgingIsNull :: Int -> Matrix Integer -> Bool
edgingIsNull s m = getAll $ foldWithIndices (edgingIndices s m) (\_ i a -> a <> All (i == 0)) (All True) m

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            when x (f >> go)

nullifyEdging :: Int -> State Triple ()
nullifyEdging s = do
  whileM_ (gets (not . edgingIsNull s . middle)) $ do
    moveLeastEdgingToStart s
    modifyEdging s

  t <- get
  let m = middle t
      mss = M.getElem s s m
  when (mss < 0) $ modify $ negateRow s

  ensureAllDivide s

ensureAllDivide :: Int -> State Triple ()
ensureAllDivide s = do
  t <- get
  let m = middle t
      mss = M.getElem s s m
  let (remainder, (blockR, blockC)) = findSmallestRemainder mss $ lowerRightBlock (s + 1) $ middle t
      smallestR = s + blockR
      smallestC = s + blockC

  when (s /= M.nrows m && s /= M.ncols m && remainder /= mss) $ do
    modify $ addRowMultiple s 1 smallestR
    t <- get
    let q = M.getElem s smallestC (middle t) `quot` mss
    modify $ addColMultiple smallestC (-q) s
    modify $ swapCols s smallestC

    nullifyEdging s

smithNormalForm :: Matrix Integer -> Triple
smithNormalForm m = flip execState (matrixToTriple m) $ do
    forM_ [1 .. min (M.ncols m) (M.nrows m)] $ \s -> do
      t <- get
      unless (matrixIsNull $ lowerRightBlock s $ middle t) $ do
        moveLeastToStart s
        modifyEdging s
        nullifyEdging s
