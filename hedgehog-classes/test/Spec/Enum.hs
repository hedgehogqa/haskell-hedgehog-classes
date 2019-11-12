module Spec.Enum (testEnum, testBoundedEnum) where

import Hedgehog
import Hedgehog.Classes

import Data.Int (Int64)
import Data.Word (Word64)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)

testEnum :: [(String, [Laws])]
testEnum =
  [ ("Integer", listInteger)
  , ("Natural", listNatural)
  ]

testBoundedEnum :: [(String, [Laws])]
testBoundedEnum =
  [ ("E", listE)
  , ("Int", listInt)
  , ("Int8", listInt8)
  , ("Int16", listInt16)
  , ("Int32", listInt32)
  , ("Int64", listInt64)
  , ("Word", listWord)
  , ("Word8", listWord8)
  , ("Word16", listWord16)
  , ("Word32", listWord32)
  , ("Word64", listWord64) 
  ]

listE :: [Laws]
listE = [boundedEnumLaws genE]

data E = E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8
  deriving (Eq, Show, Enum, Bounded)

genE :: Gen E
genE = Gen.frequency
  [ (1, pure E1)
  , (1, pure E2)
  , (1, pure E3)
  , (1, pure E4)
  , (1, pure E5)
  , (1, pure E6)
  , (1, pure E7)
  , (1, pure E8)
  ]

ranged :: (Bounded a, Num a) => (Range.Range a -> b) -> b
ranged f = f (Range.constantBounded)

listInt, listInt8, listInt16, listInt32, listInt64 :: [Laws]
listInt = [boundedEnumLaws (ranged Gen.int)]
listInt8 = [boundedEnumLaws (ranged Gen.int8)]
listInt16 = [boundedEnumLaws (ranged Gen.int16)]
listInt32 = [boundedEnumLaws (ranged Gen.int32)]
listInt64 = [boundedEnumLaws (ranged Gen.int64)]

listWord, listWord8, listWord16, listWord32, listWord64 :: [Laws]
listWord = [boundedEnumLaws (ranged Gen.word)]
listWord8 = [boundedEnumLaws (ranged Gen.word8)]
listWord16 = [boundedEnumLaws (ranged Gen.word16)]
listWord32 = [boundedEnumLaws (ranged Gen.word32)]
listWord64 = [boundedEnumLaws (ranged Gen.word64)]

listInteger, listNatural :: [Laws]
listInteger = [enumLaws (Gen.integral $ Range.constantFrom
    (0 :: Integer)
    (2 * fromIntegral (minBound :: Int64))
    (2 * fromIntegral (maxBound :: Int64)))]
listNatural = [enumLaws (Gen.integral $ Range.constant
    (0 :: Natural)
    (2 * fromIntegral (maxBound :: Word64)))]
