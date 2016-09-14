module Math.Algebra.SmithNormalForm (
  Triple(..),
  smithNormalForm
) where

import Data.Monoid (All(..), (<>))
import Data.List (foldl')
import Control.Monad.State
import Debug.Trace

import Data.Matrix (Matrix)
import qualified Data.Matrix as M


-- This entire file could benefit from being more lensy
-- And more recursive, although if we are looking for speed
-- it should just be rewritten in C.

--------------------------------------------------------------------------------
-- Utilities missing from Data.Matrix

combineCols :: Num a => Int -> a -> Int -> Matrix a -> Matrix a
combineCols c1 l c2 m = M.mapCol (\j x -> x + l * M.getElem j c2 m) c1 m

scaleCol :: Num a => a -> Int -> Matrix a -> Matrix a
scaleCol = M.mapCol . const . (*)

foldWithIndices :: [(Int, Int)] -> ((Int, Int) -> a -> b -> b) -> b -> Matrix a -> b
foldWithIndices is f s m = foldl' (\b (i, j) -> f (i, j) (M.unsafeGet i j m) b) s is

foldWithIndex :: ((Int, Int) -> a -> b -> b) -> b -> Matrix a -> b
foldWithIndex f s m = foldWithIndices [ (i, j) | j <- [1 .. M.ncols m],  i <- [1 .. M.nrows m] ] f s m

--------------------------------------------------------------------------------
-- Triples
-- These keep track of a (L, M, R) decomposition of a matrix

-- Could be generalised to any PID
data Triple = Triple { left   :: Matrix Integer,
                       middle :: Matrix Integer,
                       right  :: Matrix Integer }
  deriving (Show, Eq)

matrixToTriple :: Matrix Integer -> Triple
matrixToTriple m = Triple (M.identity $ M.nrows m) m (M.identity $ M.ncols m)

swapRows :: Int -> Int -> Triple -> Triple
swapRows i j (Triple l m r) = Triple (M.switchCols i j l) (M.switchRows i j m) r

swapCols :: Int -> Int -> Triple -> Triple
swapCols i j (Triple l m r) = Triple l (M.switchCols i j m) (M.switchRows i j r)

addRowMultiple :: Int -> Integer -> Int -> Triple -> Triple
addRowMultiple i x j (Triple l m r) = Triple (combineCols j (-x) i l) (M.combineRows i x j m) r

addColMultiple :: Int -> Integer -> Int -> Triple -> Triple
addColMultiple i x j (Triple l m r) = Triple l (combineCols i x j m) (M.combineRows j (-x) i r)

negateRow :: Int -> Triple -> Triple
negateRow i (Triple l m r) = Triple (M.mapCol (const negate) i l) (M.mapRow (const negate) i m) r

--------------------------------------------------------------------------------
-- Smith Normal Form
-- (There are almost certainly more efficient algorithms)

lowerRightBlock :: Int -> Matrix a -> Matrix a
lowerRightBlock s m = M.submatrix s r s c m
  where r = M.nrows m
        c = M.ncols m

findSmallest :: (Num a, Ord a) => Matrix a -> (a, (Int, Int))
findSmallest m = foldWithIndex f (M.unsafeGet 1 1 m, (1, 1)) m
  where f newIndices new (smallest, smallestIndices) =
          if (smallest == 0) || (new /= 0 && abs new < abs smallest) then (new, newIndices)
          else (smallest, smallestIndices)

findSmallestRemainder :: Integer -> Matrix Integer -> (Integer, (Int, Int))
findSmallestRemainder x m = foldWithIndex f (x, (1, 1)) m
  where f newIndices new (smallest, smallestIndices) =
          let r = new `rem` x in
          if r /= 0 && r < smallest then (new, newIndices)
          else (smallest, smallestIndices)

edgingIndices :: Int -> Matrix a -> [(Int, Int)]
edgingIndices s m = [ (i, s) | i <- [s+1 .. M.nrows m]] ++ [ (s, j) | j <- [s+1 .. M.ncols m]]

findSmallestInEdging :: (Num a, Ord a) => Int -> Matrix a -> (a, (Int, Int))
findSmallestInEdging s m = foldWithIndices (edgingIndices s m) f (M.unsafeGet s s m, (s, s)) m
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
      mss = M.unsafeGet s s m
  forM_ [s+1 .. M.nrows m] $ \i ->
    when (M.unsafeGet i s m /= 0) $ modify $
      let q = M.unsafeGet i s m `quot` mss in
      addRowMultiple i (-q) s

  forM_ [s+1 .. M.ncols m] $ \j ->
    when (M.unsafeGet s j m /= 0) $ modify $
      let q = M.unsafeGet s j m `quot` mss in
      addColMultiple j (-q) s


moveLeastEdgingToStart :: Int -> State Triple ()
moveLeastEdgingToStart s = do
  t <- get
  let (_, (smallestR, smallestC)) = findSmallestInEdging s $ middle t
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
            if x then f >> go
            else return ()

nullifyEdging :: Int -> State Triple ()
nullifyEdging s = do
  whileM_ (do t <- get; return (not $ edgingIsNull s (middle t))) $ do
    moveLeastEdgingToStart s
    modifyEdging s

  t <- get
  let m = middle t
      mss = M.unsafeGet s s m
  when (mss < 0) $ modify $ negateRow s

  ensureAllDivide s

ensureAllDivide :: Int -> State Triple ()
ensureAllDivide s = do
  t <- get
  let m = middle t
      mss = M.unsafeGet s s m
  let (_, (blockR, blockC)) = findSmallestRemainder mss $ lowerRightBlock (s + 1) $ middle t
      smallestR = s + blockR
      smallestC = s + blockC

  when (s /= M.nrows m && s /= M.ncols m && blockR /= 1 && blockC /= 1) $ do
    modify $ addRowMultiple s 1 smallestR
    t <- get
    let q = M.unsafeGet s smallestC (middle t) `quot` mss
    modify $ addColMultiple smallestC (-q) s
    modify $ swapCols s smallestC

    nullifyEdging s


smithNormalForm :: Matrix Integer -> Triple
smithNormalForm m = flip execState (matrixToTriple m) $ do
    forM_ [1 .. min (M.ncols m) (M.nrows m)] $ \s -> do
      t <- get
      when (not $ matrixIsNull $ lowerRightBlock s $ middle t) $ do
        moveLeastToStart s
        modifyEdging s
        nullifyEdging s
