{-# LANGUAGE DeriveGeneric #-}

module Spec.Json (testJson) where

import Hedgehog
import Hedgehog.Classes
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

testJson :: [(String, [Laws])]
testJson =
  [ ("Person", listPerson)
  ]

data Person = Person { name :: String, age :: Int }
  deriving (Eq, Show, Generic)

instance FromJSON Person where
instance ToJSON Person where

listPerson :: [Laws]
listPerson = [jsonLaws genPerson]

genPerson :: Gen Person
genPerson = Person <$> (Gen.string (Range.linear 3 7) Gen.alpha) <*> (Gen.int (Range.linear 0 65))
