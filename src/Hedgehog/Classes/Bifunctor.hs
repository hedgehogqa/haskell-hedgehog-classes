{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Bifunctor (bifunctorLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Data.Bifunctor (Bifunctor(..))

bifunctorLaws :: forall f.
  ( Bifunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Laws
bifunctorLaws gen = Laws "Bifunctor"
  [ ("Identity", bifunctorIdentity gen)
  , ("First Identity", bifunctorFirstIdentity gen)
  , ("Second Identity", bifunctorSecondIdentity gen)
  , ("Bifunctor Composition", bifunctorComposition gen) 
  ]

bifunctorIdentity :: forall f.
  ( Bifunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property
bifunctorIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  bimap id id x `heq2` x 

bifunctorFirstIdentity :: forall f.
  ( Bifunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property
bifunctorFirstIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  first id x `heq2` x

bifunctorSecondIdentity :: forall f.
  ( Bifunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property
bifunctorSecondIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  second id x `heq2` x

bifunctorComposition :: forall f.
  ( Bifunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property
bifunctorComposition fgen = property $ do
  z <- forAll $ fgen genSmallInteger genSmallInteger
  bimap id id z `heq2` ((first id . second id) z)

