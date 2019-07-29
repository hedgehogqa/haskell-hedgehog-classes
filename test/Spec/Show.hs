module Spec.Show
  ( testShow
  , testShowRead
  ) where

import Hedgehog
import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

testShow :: [(String, [Laws])]
testShow =
  [ ("E", lawsE)
  , ("Int", lawsInt)
  , ("Int8", lawsInt8)
  , ("Int16", lawsInt16)
  , ("Int32", lawsInt32)
  , ("Int64", lawsInt64)
  , ("Word", lawsWord)
  , ("Word8", lawsWord8)
  , ("Word16", lawsWord16)
  , ("Word32", lawsWord32)
  , ("Word64", lawsWord64) 
  ]

testShowRead :: [(String, [Laws])]
testShowRead =
  [ ("E", readLawsE)
  , ("Int", readLawsInt)
  , ("Int8", readLawsInt8)
  , ("Int16", readLawsInt16)
  , ("Int32", readLawsInt32)
  , ("Int64", readLawsInt64)
  , ("Word", readLawsWord)
  , ("Word8", readLawsWord8)
  , ("Word16", readLawsWord16)
  , ("Word32", readLawsWord32)
  , ("Word64", readLawsWord64) 
  ]

lawsE, readLawsE :: [Laws]
lawsE = [showLaws genE]
readLawsE = [showReadLaws genE]

data E = E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8
  deriving (Eq, Show, Read, Enum, Bounded)

genE :: Gen E
genE = Gen.enumBounded

ranged :: (Bounded a, Num a) => (Range.Range a -> b) -> b
ranged f = f (Range.constantBounded)

lawsInt, lawsInt8, lawsInt16, lawsInt32, lawsInt64 :: [Laws]
lawsInt = [showLaws (ranged Gen.int)]
lawsInt8 = [showLaws (ranged Gen.int8)]
lawsInt16 = [showLaws (ranged Gen.int16)]
lawsInt32 = [showLaws (ranged Gen.int32)]
lawsInt64 = [showLaws (ranged Gen.int64)]

lawsWord, lawsWord8, lawsWord16, lawsWord32, lawsWord64 :: [Laws]
lawsWord = [showLaws (ranged Gen.word)]
lawsWord8 = [showLaws (ranged Gen.word8)]
lawsWord16 = [showLaws (ranged Gen.word16)]
lawsWord32 = [showLaws (ranged Gen.word32)]
lawsWord64 = [showLaws (ranged Gen.word64)]

readLawsInt, readLawsInt8, readLawsInt16, readLawsInt32, readLawsInt64 :: [Laws]
readLawsInt = [showReadLaws (ranged Gen.int)]
readLawsInt8 = [showReadLaws (ranged Gen.int8)]
readLawsInt16 = [showReadLaws (ranged Gen.int16)]
readLawsInt32 = [showReadLaws (ranged Gen.int32)]
readLawsInt64 = [showReadLaws (ranged Gen.int64)]

readLawsWord, readLawsWord8, readLawsWord16, readLawsWord32, readLawsWord64 :: [Laws]
readLawsWord = [showReadLaws (ranged Gen.word)]
readLawsWord8 = [showReadLaws (ranged Gen.word8)]
readLawsWord16 = [showReadLaws (ranged Gen.word16)]
readLawsWord32 = [showReadLaws (ranged Gen.word32)]
readLawsWord64 = [showReadLaws (ranged Gen.word64)]
