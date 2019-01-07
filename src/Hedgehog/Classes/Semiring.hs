{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Semiring (semiringLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Prelude hiding (Num(..))
import Data.Semiring

semiringLaws :: (Semiring a, Eq a, Show a) => Gen a -> Laws
semiringLaws gen = Laws "Semiring"
  [
  ]

type SemiringProp a = (Semiring a, Eq a, Show a) => Gen a -> Property

semiringLeftMultiplicationDistributes :: forall a. SemiringProp a
semiringLeftMultiplicationDistributes gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  (a * (b + c)) === ((a * b) + (a * c))
 
semiringRightMultiplicationDistributes :: forall a. SemiringProp a
semiringRightMultiplicationDistributes gen = property $ do
  ((a + b) * c) === 
semiringLeftIdentityPlus :: forall a. SemiringProp a
semiringLeftIdentityPlus gen = property $ do

semiringRightIdentityPlus :: forall a. SemiringProp a
semiringRightIdentityPlus gen = property $ do

semiringLeftIdentityTimes :: forall a. SemiringProp a
semiringLeftIdentityTimes gen = property $ do

semiringRightIdentityTimes :: forall a. SemiringProp a
semiringRightIdentityTimes gen = property $ do

semiringLeftAnnihilation :: forall a. SemiringProp a
semiringLeftAnnihilation gen = property $ do

semiringRightAnnihilation :: forall a. SemiringProp a
semiringRightAnnihilation gen = property $ do

semiringCommutativePlus :: forall a. SemiringProp a
semiringCommutativePlus gen = property $ do

semiringAssociativeTimes :: forall a. SemiringProp a
semiringAssociativeTimes gen = property $ do
