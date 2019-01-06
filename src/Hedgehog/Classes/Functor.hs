{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Functor (functorLaws) where

import Hedgehog
import Hedgehog.Classes.Common

functorLaws ::
  ( Functor f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
functorLaws gen = Laws "Functor"
  [ ("Identity", functorIdentity gen)
  , ("Composition", functorComposition gen)
  , ("Const", functorConst gen)
  ]

functorIdentity ::
  ( Functor f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
functorIdentity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  fmap id a === id a

functorComposition ::
  ( Functor f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
functorComposition fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  (fmap func2 (fmap func1 a)) === (fmap (func2 . func1) a)

functorConst ::
  ( Functor f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
functorConst fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  (fmap (const 'X') a) === ('X' <$ a)
