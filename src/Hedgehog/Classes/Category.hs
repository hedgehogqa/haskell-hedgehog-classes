{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Category (categoryLaws, commutativeCategoryLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Control.Category(Category(..))
import Prelude hiding (id, (.))

categoryLaws :: forall f.
  ( Category f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Laws
categoryLaws gen = Laws "Category"
  [ ("Right Identity", categoryRightIdentity gen)
  , ("Left Identity", categoryLeftIdentity gen)
  , ("Associativity", categoryAssociativity gen)
  ]

commutativeCategoryLaws :: forall f.
  ( Category f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Laws
commutativeCategoryLaws gen = Laws "Commutative Category"
  [ ("Commutativity", categoryCommutativity gen)
  ]

categoryRightIdentity :: forall f.
  ( Category f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property
categoryRightIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  (x . id) `heq2` x

categoryLeftIdentity :: forall f.
  ( Category f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property
categoryLeftIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  (id . x) `heq2` x

categoryAssociativity :: forall f.
  ( Category f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property
categoryAssociativity fgen = property $ do
  f <- forAll $ fgen genSmallInteger genSmallInteger
  g <- forAll $ fgen genSmallInteger genSmallInteger 
  h <- forAll $ fgen genSmallInteger genSmallInteger 
  (f . (g . h)) `heq2` ((f . g) . h)

categoryCommutativity :: forall f.
  ( Category f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property
categoryCommutativity fgen = property $ do
  f <- forAll $ fgen genSmallInteger genSmallInteger
  g <- forAll $ fgen genSmallInteger genSmallInteger
  (f . g) `heq2` (g . f)


