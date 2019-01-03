{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Functor (functorLaws) where

import Hedgehog
import Hedgehog.Classes.Common

functorLaws ::
  ( Functor f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  , Eq a, Show a
  ) => Gen (f a) -> Laws
functorLaws gen = Laws "Functor"
  [ ("Identity", functorIdentity gen)
  , ("Composition", functorComposition gen)
  , ("Const", functorConst gen)
  ]

functorIdentity ::
  ( Functor f
  , Eq (f a), Show (f a) 
  ) => Gen (f a) -> Property
functorIdentity gen = property $ do
  a <- forAll gen
  fmap id a === id a

functorComposition ::
  ( Functor f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => Gen (f a) -> Property
functorComposition gen = property $ do
  a <- forAll $ hackReplace genSmallInteger gen
  (fmap func2 (fmap func1 a)) === (fmap (func2 . func1) a)

functorConst ::
  ( Functor f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => Gen (f a) -> Property
functorConst gen = property $ do
  a <- forAll $ hackReplace genSmallInteger gen
  (fmap (const 'X') a) === ('X' <$ a)
