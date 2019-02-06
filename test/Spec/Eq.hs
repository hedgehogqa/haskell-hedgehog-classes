module Spec.Eq (testEq) where

import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

ranged :: (Bounded a, Num a) => (Range.Range a -> b) -> b
ranged f = f (Range.constantBounded)

testEq :: [(String, [Laws])]
testEq =
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
  , ("BadEq", listBadEq)
  ]

listInt, listInt8, listInt16, listInt32, listInt64 :: [Laws]
listInt = [eqLaws (ranged Gen.int)]
listInt8 = [eqLaws (ranged Gen.int8)]
listInt16 = [eqLaws (ranged Gen.int16)]
listInt32 = [eqLaws (ranged Gen.int32)]
listInt64 = [eqLaws (ranged Gen.int64)]

listWord, listWord8, listWord16, listWord32, listWord64 :: [Laws]
listWord = [eqLaws (ranged Gen.word)]
listWord8 = [eqLaws (ranged Gen.word8)]
listWord16 = [eqLaws (ranged Gen.word16)]
listWord32 = [eqLaws (ranged Gen.word32)]
listWord64 = [eqLaws (ranged Gen.word64)]

listBadEq :: [Laws]
listBadEq = [ eqLaws $ pure BadReflexive ]
  ++ [ eqLaws $ Gen.frequency [(1, pure BadSymmetric1),(1,pure BadSymmetric2)] ]

data BadReflexive = BadReflexive
  deriving (Show)

instance Eq BadReflexive where
  _ == _ = False

data BadSymmetric = BadSymmetric1 | BadSymmetric2
  deriving (Show)

instance Eq BadSymmetric where
  BadSymmetric1 == BadSymmetric1 = True
  BadSymmetric2 == BadSymmetric2 = True
  BadSymmetric2 == BadSymmetric1 = True
  BadSymmetric1 == BadSymmetric2 = False

