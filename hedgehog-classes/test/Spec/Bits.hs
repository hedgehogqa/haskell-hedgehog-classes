module Spec.Bits (testBits) where

import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

ranged :: (Bounded a, Num a) => (Range.Range a -> b) -> b
ranged f = f (Range.constantBounded)

testBits :: [(String, [Laws])]
testBits =
  [ ("Int", listInt)
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

listInt, listInt8, listInt16, listInt32, listInt64 :: [Laws]
listInt = [bitsLaws (ranged Gen.int)]
listInt8 = [bitsLaws (ranged Gen.int8)]
listInt16 = [bitsLaws (ranged Gen.int16)]
listInt32 = [bitsLaws (ranged Gen.int32)]
listInt64 = [bitsLaws (ranged Gen.int64)]

listWord, listWord8, listWord16, listWord32, listWord64 :: [Laws]
listWord = [bitsLaws (ranged Gen.word)]
listWord8 = [bitsLaws (ranged Gen.word8)]
listWord16 = [bitsLaws (ranged Gen.word16)]
listWord32 = [bitsLaws (ranged Gen.word32)]
listWord64 = [bitsLaws (ranged Gen.word64)]
