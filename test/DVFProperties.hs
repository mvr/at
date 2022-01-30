-- |
module DVFProperties where

import Test.Hspec

import Control.Monad (forM_)

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.DVF

checkValidResult :: (DVF a, Show (Basis a)) => a -> Basis a -> Expectation
checkValidResult a b = case vf a b of
    Source b' _ -> (b, b') `shouldSatisfy` (isBasis a . snd)
    Target b' _ -> (b, b') `shouldSatisfy` (isBasis a . snd)
    Critical    -> return ()

checkSourceTarget :: (DVF a, Show (Basis a)) => a -> Basis a -> Expectation
checkSourceTarget a b = case vf a b of
  Source t _ -> case vf a t of
    Source b' _ -> expectationFailure $ "Target of " ++ show b ++ " ~> " ++ show t ++ " was also source of " ++ show t ++ " ~> " ++ show b'
    Target b' _ -> b `shouldBe` b'
    Critical    -> expectationFailure $ "Target of " ++ show b ++ " ~> " ++ show t ++ " was critical"
  Target s _ -> case vf a s of
    Source b' _ -> b `shouldBe` b'
    Target b' _ -> expectationFailure $ "Source of " ++ show s ++ " ~> " ++ show b ++ " was also target"
    Critical    -> expectationFailure $ "Source of " ++ show s ++ " ~> " ++ show b ++ " was critical"
  Critical -> return ()

-- check it's an actual face
-- check incidence is correct

check :: (FiniteType a, DVF a, Show (Basis a)) => Int -> a -> Spec
check n a = do
  it "vf should yield valid partners" $
    forM_ [0 .. n] (\i -> forM_ (basis a i) (checkValidResult a))
  it "vf should be a bijection" $
    forM_ [0 .. n] (\i -> forM_ (basis a i) (checkSourceTarget a))
