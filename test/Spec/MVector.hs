{-# LANGUAGE CPP #-}

#if !HAVE_VECTOR

module Spec.MVector (testMUVector) where

testMUVector :: [a]
testMUVector = []

#else

module Spec.MVector (testMUVector) where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog.Classes (Laws, muvectorLaws)

testMUVector :: [(String, [Laws])]
testMUVector =
  [ ("Word8",  [muvectorLaws (Gen.word8  Range.constantBounded)])
  , ("(Int, Word)", [muvectorLaws ((,) <$> Gen.int Range.constantBounded <*> Gen.word Range.constantBounded)])
  ]

#endif
