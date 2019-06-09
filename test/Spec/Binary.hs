{-# LANGUAGE DeriveGeneric #-}

module Spec.Binary (testBinary) where

import Hedgehog
import Hedgehog.Classes
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Binary
import GHC.Generics (Generic(..))

testBinary :: [(String, [Laws])]
testBinary =
  [ ("Person", listPerson)
  ]

data Person = Person { name :: String, age :: Int }
  deriving (Eq, Show, Generic)

instance Binary Person where

listPerson :: [Laws]
listPerson = [binaryLaws genPerson]

genPerson :: Gen Person
genPerson = Person <$> (Gen.string (Range.linear 3 7) Gen.alpha) <*> (Gen.int (Range.linear 0 65))
