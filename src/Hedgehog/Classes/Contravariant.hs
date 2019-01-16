{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Contravariant (contravariantLaws) where

import Data.Functor.Contravariant (Contravariant(..))

import Hedgehog
import Hedgehog.Classes.Common

contravariantLaws ::
  ( Contravariant f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
contravariantLaws gen = Laws "Contravariant"
  [ ("Identity", contravariantIdentity gen)
  , ("Composition", contravariantComposition gen)
  ]

contravariantIdentity ::
  ( Contravariant f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
contravariantIdentity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  contramap id a `heq1` id a

contravariantComposition ::
  ( Contravariant f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
contravariantComposition fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  f' <- forAll genQuadraticEquation
  g' <- forAll genQuadraticEquation
  let f = runQuadraticEquation f'
  let g = runQuadraticEquation g'
  (contramap f (contramap g a)) `heq1` (contramap (g . f) a)

