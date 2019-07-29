module Spec.Storable (testStorable) where

import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

testStorable :: [(String, [Laws])]
testStorable =
  [ ("Int", lawsInt)
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

ranged :: (Bounded a, Num a) => (Range.Range a -> b) -> b
ranged f = f (Range.constantBounded)

lawsInt, lawsInt8, lawsInt16, lawsInt32, lawsInt64 :: [Laws]
lawsInt = [storableLaws (ranged Gen.int)]
lawsInt8 = [storableLaws (ranged Gen.int8)]
lawsInt16 = [storableLaws (ranged Gen.int16)]
lawsInt32 = [storableLaws (ranged Gen.int32)]
lawsInt64 = [storableLaws (ranged Gen.int64)]

lawsWord, lawsWord8, lawsWord16, lawsWord32, lawsWord64 :: [Laws]
lawsWord = [storableLaws (ranged Gen.word)]
lawsWord8 = [storableLaws (ranged Gen.word8)]
lawsWord16 = [storableLaws (ranged Gen.word16)]
lawsWord32 = [storableLaws (ranged Gen.word32)]
lawsWord64 = [storableLaws (ranged Gen.word64)]
