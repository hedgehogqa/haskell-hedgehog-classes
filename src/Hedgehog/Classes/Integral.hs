{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Integral (integralLaws) where

import Hedgehog
import Hedgehog.Classes.Common

integralLaws :: (Integral a, Show a) => Gen a -> Laws
integralLaws gen = Laws "Integral"
  [ ("Quotient Remainder", integralQuotientRemainder gen)
  , ("Division Modulus", integralDivisionModulus gen)
  , ("Integer Roundtrip", integralIntegerRoundtrip gen)
  ]

integralQuotientRemainder :: forall a. (Integral a, Show a) => Gen a -> Property
integralQuotientRemainder gen = property $ do
  x <- forAll gen
  y <- forAll gen
  (quot x y) * y + (rem x y) === x

integralDivisionModulus :: forall a. (Integral a, Show a) => Gen a -> Property
integralDivisionModulus gen = property $ do
  x <- forAll gen
  y <- forAll gen
  (div x y) * y + (mod x y) === x

integralIntegerRoundtrip :: forall a. (Integral a, Show a) => Gen a -> Property
integralIntegerRoundtrip gen = property $ do
  x <- forAll gen
  fromInteger (toInteger x) === x