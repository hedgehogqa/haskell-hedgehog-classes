{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Json (jsonLaws) where

import Hedgehog
import Hedgehog.Classes.Common
import Data.Aeson (FromJSON, ToJSON(toJSON))
import qualified Data.Aeson as Aeson

jsonLaws :: (FromJSON a, ToJSON a, Eq a, Show a) => Gen a -> Laws
jsonLaws gen = Laws "ToJSON/FromJSON"
  [ ("Partial Isomorphism", jsonEncodingPartialIsomorphism gen)
  , ("Encoding equals value", jsonEncodingEqualsValue gen)
  ]

jsonEncodingPartialIsomorphism :: forall a. (ToJSON a, FromJSON a, Show a, Eq a) => Gen a -> Property
jsonEncodingPartialIsomorphism gen = property $ do
  x <- forAll gen
  Aeson.decode (Aeson.encode x) === Just x

jsonEncodingEqualsValue :: forall a. (ToJSON a, Show a) => Gen a -> Property
jsonEncodingEqualsValue gen = property $ do
  x <- forAll gen
  case Aeson.decode (Aeson.encode x) of
    Nothing -> failure
    Just (v :: Aeson.Value) -> v === toJSON x

