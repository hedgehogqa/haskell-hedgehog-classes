{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Enum (enumLaws, boundedEnumLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import qualified Hedgehog.Gen as Gen

enumLaws :: (Enum a, Eq a, Show a) => Gen a -> Laws
enumLaws gen = Laws "Enum"
  [ ("Succ Pred Identity", succPredIdentity gen)
  , ("Pred Succ Identity", predSuccIdentity gen)
  ]

boundedEnumLaws :: (Bounded a, Enum a, Eq a, Show a) => Gen a -> Laws
boundedEnumLaws gen = Laws "Bounded Enum"
  [ ("Succ Pred Identity", succPredBoundedIdentity gen)
  , ("Pred Succ Identity", predSuccBoundedIdentity gen)
  ]

succPredIdentity :: forall a. (Enum a, Eq a, Show a) => Gen a -> Property
succPredIdentity gen = property $ do
  x <- forAll gen
  succ (pred x) `heq` x

predSuccIdentity :: forall a. (Enum a, Eq a, Show a) => Gen a -> Property
predSuccIdentity gen = property $ do
  x <- forAll gen
  pred (succ x) `heq` x

succPredBoundedIdentity :: forall a. (Bounded a, Enum a, Eq a, Show a) => Gen a -> Property
succPredBoundedIdentity gen = property $ do
  x <- forAll $ Gen.filter (/= minBound) gen
  succ (pred x) `heq` x

predSuccBoundedIdentity :: forall a. (Bounded a, Enum a, Eq a, Show a) => Gen a -> Property
predSuccBoundedIdentity gen = property $ do
  x <- forAll $ Gen.filter (/= maxBound) gen
  pred (succ x) `heq` x
