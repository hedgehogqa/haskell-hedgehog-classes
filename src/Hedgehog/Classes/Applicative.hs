{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Applicative (applicativeLaws) where

import Control.Applicative (Applicative(..))

import Hedgehog
import Hedgehog.Classes.Common

applicativeLaws ::
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  , Eq a, Show a
  ) => Gen (f a) -> Laws
applicativeLaws gen = Laws "Applicative"
  [ ("Identity", applicativeIdentity gen)
  , ("Composition", applicativeComposition gen)
  , ("Homomorphism", applicativeHomomorphism gen)
  , ("Interchange", applicativeInterchange gen)
  , ("LiftA2 Part 1", applicativeLiftA2_1 gen)
  -- todo: liftA2 part 2, we need an equation of two variables for this
  ]

applicativeIdentity :: forall f a.
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  , Eq a, Show a
  ) => Gen (f a) -> Property
applicativeIdentity gen = property $ do
  a <- forAll gen
  (pure id <*> a) === a

applicativeComposition :: forall f a.
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => Gen (f a) -> Property
applicativeComposition gen = property $ do
  u' <- forAll $ hackReplace genQuadraticEquation gen
  v' <- forAll $ hackReplace genQuadraticEquation gen
  w' <- forAll genSmallInteger

  let u = runQuadraticEquation <$> u'
      v = runQuadraticEquation <$> v'
      w = pure w'
  (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

applicativeHomomorphism :: forall f a.
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => Gen (f a) -> Property
applicativeHomomorphism _ = property $ do
  e <- forAll genQuadraticEquation
  a <- forAll genSmallInteger
  let f = runQuadraticEquation e
  (pure f <*> pure a) === (pure (f a) :: f Integer)

applicativeInterchange :: forall f a.
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => Gen (f a) -> Property
applicativeInterchange gen = property $ do
  u' <- forAll $ hackReplace genQuadraticEquation gen
  y <- forAll genSmallInteger
  let u = fmap runQuadraticEquation u'
  (u <*> pure y) === (pure ($ y) <*> u)

applicativeLiftA2_1 :: forall f a.
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => Gen (f a) -> Property
applicativeLiftA2_1 gen = property $ do
  f' <- forAll $ hackReplace genQuadraticEquation gen
  x <- forAll $ hackReplace genSmallInteger gen
  let f = fmap runQuadraticEquation f'
  (liftA2 id f x) === (f <*> x)

