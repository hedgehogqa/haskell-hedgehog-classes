module Spec.Integral (testIntegral) where

import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

ranged :: (Bounded a, Integral a) => (Range.Range a -> b) -> b
ranged f = f (Range.linear 1 maxBound)

testIntegral :: [(String, [Laws])]
testIntegral =
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
listInt = [integralLaws (ranged Gen.int)]
listInt8 = [integralLaws (ranged Gen.int8)]
listInt16 = [integralLaws (ranged Gen.int16)]
listInt32 = [integralLaws (ranged Gen.int32)]
listInt64 = [integralLaws (ranged Gen.int64)]

listWord, listWord8, listWord16, listWord32, listWord64 :: [Laws]
listWord = [integralLaws (ranged Gen.word)]
listWord8 = [integralLaws (ranged Gen.word8)]
listWord16 = [integralLaws (ranged Gen.word16)]
listWord32 = [integralLaws (ranged Gen.word32)]
listWord64 = [integralLaws (ranged Gen.word64)]
