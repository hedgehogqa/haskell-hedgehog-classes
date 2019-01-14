module Spec.Ix (testIx) where

import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

ranged :: (Bounded a, Num a) => (Range.Range a -> b) -> b
ranged f = f (Range.constantBounded)

testIx :: [(String, [Laws])]
testIx =
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
  , ("Bool", listBool)
  ]

listInt, listInt8, listInt16, listInt32, listInt64 :: [Laws]
listInt = [ixLaws (ranged Gen.int)]
listInt8 = [ixLaws (ranged Gen.int8)]
listInt16 = [ixLaws (ranged Gen.int16)]
listInt32 = [ixLaws (ranged Gen.int32)]
listInt64 = [ixLaws (ranged Gen.int64)]

listWord, listWord8, listWord16, listWord32, listWord64 :: [Laws]
listWord = [ixLaws (ranged Gen.word)]
listWord8 = [ixLaws (ranged Gen.word8)]
listWord16 = [ixLaws (ranged Gen.word16)]
listWord32 = [ixLaws (ranged Gen.word32)]
listWord64 = [ixLaws (ranged Gen.word64)]

listBool :: [Laws]
listBool = [ixLaws Gen.bool]
