{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Ord (ordLaws) where

import Hedgehog
import Hedgehog.Classes.Common

ordLaws :: forall a. (Ord a, Show a) => Gen a -> Laws
ordLaws gen = Laws "Ord"
  [ ("Antisymmetry", ordAntisymmetric gen)
  , ("Transitivity", ordTransitive gen)
  , ("Totality", ordTotal gen) 
  ]

ordAntisymmetric :: forall a. (Ord a, Show a) => Gen a -> Property
ordAntisymmetric gen = property $ do
  a <- forAll gen
  b <- forAll gen
  ((a <= b) && (b <= a)) === (a == b)

ordTransitive :: forall a. (Ord a, Show a) => Gen a -> Property
ordTransitive gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  case (compare a b, compare b c) of
    (LT,LT) -> a `hLessThan` c
    (LT,EQ) -> a `hLessThan` c
    (LT,GT) -> success
    (EQ,LT) -> a `hLessThan` c
    (EQ,EQ) -> a === c
    (EQ,GT) -> a `hGreaterThan` c
    (GT,LT) -> success
    (GT,EQ) -> a `hGreaterThan` c
    (GT,GT) -> a `hGreaterThan` c

ordTotal :: forall a. (Ord a, Show a) => Gen a -> Property
ordTotal gen = property $ do
  a <- forAll gen
  b <- forAll gen
  ((a <= b) || (b <= a)) === True
