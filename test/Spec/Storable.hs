module Spec.Storable (testStorable) where

import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (nullPtr, castPtr, plusPtr, minusPtr, alignPtr)
import Foreign.Storable (Storable, sizeOf, alignment, peek, peekByteOff, poke, pokeByteOff)

import Hedgehog (Gen)
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
  , ("complex struct", lawsStruct)
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

lawsStruct :: [Laws]
lawsStruct = [storableLaws genStruct]

genStruct :: Gen TestStruct
genStruct = TestStruct
    <$> fmap fromIntegral (Gen.integral Range.linearBounded :: Gen CInt)
    <*> Gen.string (Range.linear 0 16) (Gen.filter (/= '\NUL') Gen.latin1)

data TestStruct = TestStruct
    { testPadding :: Int
    , testString :: String
    }
  deriving (Eq, Show)
instance Storable TestStruct where
    sizeOf _ = offsetTest + (sizeOf (undefined :: Int) `max` sizeOf (undefined :: CString))
    alignment _ = alignment (undefined :: Int) `lcm` alignment (undefined :: CString)
    peek ptr = do
        pad <- peek $ castPtr ptr
        strPtr <- peekByteOff ptr offsetTest
        str <- if strPtr == nullPtr
            then return ""
            else peekCString strPtr
        return $ TestStruct
            { testPadding = pad
            , testString = str
            }
    poke ptr x = do
        poke (castPtr ptr) $ testPadding x
        strPtr <- newCString $ testString x
        pokeByteOff ptr offsetTest strPtr

offsetTest :: Int
offsetTest = (nullPtr `plusPtr` sizeOf int) `alignPtr` alignment string `minusPtr` nullPtr
  where int = undefined :: Int
        string = undefined :: CString
