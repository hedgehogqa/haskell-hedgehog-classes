{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Monoid (monoidLaws, commutativeMonoidLaws) where

import Hedgehog
import Hedgehog.Classes.Common
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

monoidLaws :: (Eq a, Monoid a, Show a) => Gen a -> Laws
monoidLaws gen = Laws "Monoid"
  [ ("Associativity", monoidAssociative gen)
  , ("Left Identity", monoidLeftIdentity gen)
  , ("Right Identity", monoidRightIdentity gen)
  , ("Concatenation", monoidConcatenation gen)
  ]

commutativeMonoidLaws :: (Eq a, Monoid a, Show a) => Gen a -> Laws
commutativeMonoidLaws gen = Laws "Commutative Monoid"
  [ ("Commutativity", monoidCommutative gen)
  ]

monoidConcatenation :: forall a. (Eq a, Monoid a, Show a) => Gen a -> Property
monoidConcatenation gen = property $ do
  as <- forAll $ Gen.list (Range.linear 0 6) gen
  mconcat as === foldr mappend mempty as

monoidAssociative :: forall a. (Eq a, Monoid a, Show a) => Gen a -> Property
monoidAssociative gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  mappend a (mappend b c) === mappend (mappend a b) c

monoidLeftIdentity :: forall a. (Eq a, Monoid a, Show a) => Gen a -> Property
monoidLeftIdentity gen = property $ do
  a <- forAll gen
  mappend mempty a === a

monoidRightIdentity :: forall a. (Eq a, Monoid a, Show a) => Gen a -> Property
monoidRightIdentity gen = property $ do
  a <- forAll gen
  mappend a mempty === a

monoidCommutative :: forall a. (Eq a, Monoid a, Show a) => Gen a -> Property
monoidCommutative gen = property $ do
  a <- forAll gen
  b <- forAll gen
  mappend a b === mappend b a
