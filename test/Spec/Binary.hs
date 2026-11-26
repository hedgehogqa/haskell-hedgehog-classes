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
--  , ("BadBinary", listBadBinary)
  ]

data Person = Person { name :: String, age :: Int }
  deriving (Eq, Show, Generic)

instance Binary Person where

listPerson :: [Laws]
listPerson = [binaryLaws genPerson]

genPerson :: Gen Person
genPerson = Person <$> (Gen.string (Range.linear 3 7) Gen.alpha) <*> (Gen.int (Range.linear 0 65))

{-
listBadBinary :: [Laws]
listBadBinary = [binaryLaws $ pure Padded]

-- 'put' writes a byte that 'get' doesn't read.
data Padded = Padded
  deriving (Eq, Show)

instance Binary Padded where
  put Padded = putWord8 0
  get = pure Padded
-}
