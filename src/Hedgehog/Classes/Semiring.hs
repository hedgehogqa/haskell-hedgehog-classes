{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Semiring (semiringLaws, ringLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Prelude hiding (Num(..))
import Data.Semiring

semiringLaws :: (Semiring a, Eq a, Show a) => Gen a -> Laws
semiringLaws gen = Laws "Semiring"
  [ ("Additive Left Identity", semiringAdditiveLeftIdentity gen)
  , ("Additive Right Identity", semiringAdditiveRightIdentity gen)
  , ("Additive Associativity", semiringAdditiveAssociativity gen)
  , ("Additive Commutativity", semiringAdditiveCommutativity gen)
  , ("Multiplicative Left Identity", semiringMultiplicativeLeftIdentity gen)
  , ("Multiplicative Right Identity", semiringMultiplicativeRightIdentity gen)
  , ("Multiplicative Associativity", semiringMultiplicativeAssociativity gen)
  , ("Multiplication Left-Distributes Over Addition", semiringLeftMultiplicationDistributes gen)
  , ("Multiplication Right-Distributes Over Addition", semiringRightMultiplicationDistributes gen)
  , ("Left Annihilation", semiringLeftAnnihilation gen) 
  , ("Right Annihilation", semiringRightAnnihilation gen) 
  ]

ringLaws :: (Ring a, Eq a, Show a) => Gen a -> Laws
ringLaws gen = Laws "Ring"
  [ ("Additive Inverse", ringAdditiveInverse gen)
  ]

type SemiringProp a = (Semiring a, Eq a, Show a) => Gen a -> Property
type RingProp a = (Ring a, Eq a, Show a) => Gen a -> Property

ringAdditiveInverse :: forall a. RingProp a
ringAdditiveInverse gen = property $ do
  a <- forAll gen
  negate a + a === zero

semiringAdditiveLeftIdentity :: forall a. SemiringProp a
semiringAdditiveLeftIdentity gen = property $ do
  x <- forAll gen
  zero + x === x

semiringAdditiveRightIdentity :: forall a. SemiringProp a
semiringAdditiveRightIdentity gen = property $ do
  x <- forAll gen
  x + zero === x

semiringAdditiveAssociativity :: forall a. SemiringProp a
semiringAdditiveAssociativity gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  a + (b + c) === (a + b) + c

semiringAdditiveCommutativity :: forall a. SemiringProp a
semiringAdditiveCommutativity gen = property $ do
  a <- forAll gen
  b <- forAll gen
  a + b === b + a

semiringMultiplicativeLeftIdentity :: forall a. SemiringProp a
semiringMultiplicativeLeftIdentity gen = property $ do
  x <- forAll gen
  one * x === x

semiringMultiplicativeRightIdentity :: forall a. SemiringProp a
semiringMultiplicativeRightIdentity gen = property $ do
  x <- forAll gen
  x * one === x 

semiringMultiplicativeAssociativity :: forall a. SemiringProp a
semiringMultiplicativeAssociativity gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  a * (b * c) === (a * b) * c

semiringLeftMultiplicationDistributes :: forall a. SemiringProp a
semiringLeftMultiplicationDistributes gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  (a * (b + c)) === ((a * b) + (a * c))

semiringRightMultiplicationDistributes :: forall a. SemiringProp a
semiringRightMultiplicationDistributes gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  (a + b) * c === (a * c) + (b * c)

semiringLeftAnnihilation :: forall a. SemiringProp a
semiringLeftAnnihilation gen = property $ do
  x <- forAll gen
  zero * x === x

semiringRightAnnihilation :: forall a. SemiringProp a
semiringRightAnnihilation gen = property $ do
  x <- forAll gen
  x * zero === x
