-- |
module DVFProperties where

import Test.Hspec

import Control.Monad (forM_, unless)

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.DVF

checkValidResult :: (DVF a, Show (Basis a)) => a -> Basis a -> Expectation
checkValidResult a b = case vf a b of
    Source b' _ -> unless (isBasis a b') $ expectationFailure $ "Target of " ++ show b ++ " ~> " ++ show b' ++ " was not a valid basis element"
    Target b' _ -> unless (isBasis a b') $ expectationFailure $ "Source of " ++ show b' ++ " ~> " ++ show b ++ " was not a valid basis element"
    Critical    -> return ()

checkIsFace :: (DVF a, Show (Basis a)) => a -> Basis a -> Expectation
checkIsFace a b = case vf a b of
  Source b' i -> unless (diff a `onBasis` b' `coeffOf` b  == incidenceCoef i) $ expectationFailure $ "Target (from source) of " ++ show b ++ " ~> " ++ show b' ++ " has boundary " ++ show (diff a `onBasis` b')
  Target b' i -> unless (diff a `onBasis` b  `coeffOf` b' == incidenceCoef i) $ expectationFailure $ "Target (from target) of " ++ show b' ++ " ~> " ++ show b ++ " has boundary " ++ show (diff a `onBasis` b)
  Critical -> return ()

checkSourceTarget :: (DVF a, Show (Basis a)) => a -> Basis a -> Expectation
checkSourceTarget a b = case vf a b of
  Source t _ -> case vf a t of
    Source b' _ -> expectationFailure $ "Target of " ++ show b ++ " ~> " ++ show t ++ " is also source of " ++ show t ++ " ~> " ++ show b'
    Target b' _ -> b `shouldBe` b'
    Critical    -> expectationFailure $ "Target of " ++ show b ++ " ~> " ++ show t ++ " is critical"
  Target s _ -> case vf a s of
    Source b' _ -> b `shouldBe` b'
    Target b' _ -> expectationFailure $ "Source of " ++ show s ++ " ~> " ++ show b ++ " is also target"
    Critical    -> expectationFailure $ "Source of " ++ show s ++ " ~> " ++ show b ++ " is critical"
  Critical -> return ()

-- check it's an actual face
-- TODO: check incidence is correct: this may be where the loop is coming in now.

check :: (FiniteType a, DVF a, Show (Basis a)) => Int -> a -> Spec
check n a = do
  it "should yield valid partners" $
    forM_ [0 .. n] (\i -> forM_ (basis a i) (checkValidResult a))
  it "should give regular faces" $
    forM_ [0 .. n] (\i -> forM_ (basis a i) (checkIsFace a))
  it "should be a bijection" $
    forM_ [0 .. n] (\i -> forM_ (basis a i) (checkSourceTarget a))
