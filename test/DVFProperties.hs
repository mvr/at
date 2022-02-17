-- |
module DVFProperties where

import Test.Hspec

import Control.Monad (forM_, unless)

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.DVF
import Math.Algebra.Combination

checkValidResult :: (DVF a, Show (Basis a)) => a -> Basis a -> Expectation
checkValidResult a b = case vf a b of
  Source b' _ -> unless (isBasis a b') $ expectationFailure $ "Target of " ++ show b ++ " ~> " ++ show b' ++ " was not a valid basis element"
  Target b' _ -> unless (isBasis a b') $ expectationFailure $ "Source of " ++ show b' ++ " ~> " ++ show b ++ " was not a valid basis element"
  Critical -> return ()

checkIsFace :: (DVF a, Show (Basis a)) => a -> Basis a -> Expectation
checkIsFace a b = case vf a b of
  Source b' i -> unless (diff a `onBasis` b' `coeffOf` b == incidenceCoef i) $ expectationFailure $ "Target (from source) of " ++ show b ++ " ~> " ++ show b' ++ " has boundary " ++ show (diff a `onBasis` b')
  Target b' i -> unless (diff a `onBasis` b `coeffOf` b' == incidenceCoef i) $ expectationFailure $ "Target (from target) of " ++ show b' ++ " ~> " ++ show b ++ " has boundary " ++ show (diff a `onBasis` b)
  Critical -> return ()

checkSourceTarget :: (DVF a, Show (Basis a)) => a -> Basis a -> Expectation
checkSourceTarget a b = case vf a b of
  Source t _ -> case vf a t of
    Source b' _ -> expectationFailure $ "Target of " ++ show b ++ " ~> " ++ show t ++ " is also source of " ++ show t ++ " ~> " ++ show b'
    Target b' _ -> unless (b == b') $ expectationFailure $ "Was not inverse: " ++ show b ++ " ~> " ++ show t ++ " <~ " ++ show b'
    Critical -> expectationFailure $ "Target of " ++ show b ++ " ~> " ++ show t ++ " is critical"
  Target s _ -> case vf a s of
    Source b' _ -> unless (b == b') $ expectationFailure $ "Was not inverse: " ++ show b ++ " <~ " ++ show s ++ " ~> " ++ show b'
    Target b' _ -> expectationFailure $ "Source of " ++ show s ++ " ~> " ++ show b ++ " is also target"
    Critical -> expectationFailure $ "Source of " ++ show s ++ " ~> " ++ show b ++ " is critical"
  Critical -> return ()

checkOn :: (ChainComplex a, DVF a, Show (Basis a)) => a -> [Basis a] -> Spec
checkOn a bs = do
  it "should yield valid partners" $
    forM_ bs (checkValidResult a)
  it "should give regular faces" $
    forM_ bs (checkIsFace a)
  it "should be a bijection" $
    forM_ bs (checkSourceTarget a)

check :: (FiniteType a, DVF a, Show (Basis a)) => Int -> a -> Spec
check n a = do
  it "should yield valid partners" $
    forM_ [0 .. n] (\i -> forM_ (basis a i) (checkValidResult a))
  it "should give regular faces" $
    forM_ [0 .. n] (\i -> forM_ (basis a i) (checkIsFace a))
  it "should be a bijection" $
    forM_ [0 .. n] (\i -> forM_ (basis a i) (checkSourceTarget a))
