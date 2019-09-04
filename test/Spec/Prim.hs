{-# language TypeApplications #-}

module Spec.Prim (testPrim) where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog.Classes (Laws, primLaws)

testPrim :: [(String, [Laws])]
testPrim =
  [ ("Int",    [primLaws (Gen.int    Range.constantBounded)])
  , ("Int8",   [primLaws (Gen.int8   Range.constantBounded)])
  , ("Int16",  [primLaws (Gen.int16  Range.constantBounded)])
  , ("Int32",  [primLaws (Gen.int32  Range.constantBounded)])
  , ("Int64",  [primLaws (Gen.int64  Range.constantBounded)])
  , ("Word",   [primLaws (Gen.word   Range.constantBounded)])
  , ("Word8",  [primLaws (Gen.word8  Range.constantBounded)])
  , ("Word16", [primLaws (Gen.word16 Range.constantBounded)])
  , ("Word32", [primLaws (Gen.word32 Range.constantBounded)])
  , ("Word64", [primLaws (Gen.word64 Range.constantBounded)])
  ]

