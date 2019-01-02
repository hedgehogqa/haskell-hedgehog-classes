{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Eq (eqLaws) where

import Hedgehog
import Hedgehog.Classes.Common

eqLaws :: (Eq a, Show a) => Gen a -> Laws
eqLaws gen = Laws "Eq"
  [ ("Transitive", eqTransitive gen)
  , ("Symmetric", eqSymmetric gen)  
  , ("Reflexive", eqReflexive gen) 
  ]

eqTransitive :: forall a. (Eq a, Show a) => Gen a -> Property
eqTransitive gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  case a == b of
    True -> case b == c of
      True -> a === c
      False -> a /== c
    False -> case b == c of
      True -> a /== c
      False -> success

eqSymmetric :: forall a. (Eq a, Show a) => Gen a -> Property
eqSymmetric gen = property $ do
  a <- forAll gen
  b <- forAll gen
  case a == b of
    True -> b === a
    False -> b /== a

eqReflexive :: forall a. (Eq a, Show a) => Gen a -> Property
eqReflexive gen = property $ do
  a <- forAll gen
  a === a
