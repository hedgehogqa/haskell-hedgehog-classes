{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Spec.Profunctor (testProfunctor) where

import Hedgehog
import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen

import Data.Bifunctor.Joker (Joker(..))

joker :: MonadGen m
  => (forall x. m x -> m (g x))
  -> m b
  -> m (Joker g a b)
joker genG genB = do
  g <- genG genB
  pure $ Joker g

jokerMaybe :: MonadGen m => m a -> m b -> m (Joker Maybe a b)
jokerMaybe _genA genB = joker Gen.maybe genB

testProfunctor :: [(String, [Laws])]
testProfunctor =
  [ ("Joker", [profunctorLaws jokerMaybe])
  ]
