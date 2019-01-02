{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Alternative (alternativeLaws) where

import Control.Applicative (Alternative(..))

import Hedgehog
import Hedgehog.Classes.Common

alternativeLaws :: (Alternative f, Eq (f a), Show (f a)) => Gen (f a) -> Laws
alternativeLaws gen = Laws "Alternative"
  [ ("Left Identity", alternativeLeftIdentity gen)
  , ("Right Identity", alternativeRightIdentity gen)
  , ("Associativity", alternativeAssociativity gen)
  ]

alternativeLeftIdentity :: (Alternative f, Eq (f a), Show (f a)) => Gen (f a) -> Property
alternativeLeftIdentity gen = property $ do
  a <- forAll gen
  (empty <|> a) === a

alternativeRightIdentity :: (Alternative f, Eq (f a), Show (f a)) => Gen (f a) -> Property
alternativeRightIdentity gen = property $ do
  a <- forAll gen
  a === (empty <|> a)

alternativeAssociativity :: (Alternative f, Eq (f a), Show (f a)) => Gen (f a) -> Property
alternativeAssociativity gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  (a <|> (b <|> c)) === ((a <|> b) <|> c)