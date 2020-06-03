-- |
-- Module:      Hedgehog.Classes.MVector
-- Copyright:   (c) 2019-2020 Andrew Lelechenko
-- Licence:     BSD3
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if !HAVE_VECTOR

module Hedgehog.Classes.MVector () where

#else

module Hedgehog.Classes.MVector
  ( muvectorLaws
  ) where

import Control.Monad (when)
import Control.Monad.ST
import qualified Data.Vector.Generic.Mutable as MU (basicInitialize)
import qualified Data.Vector.Unboxed.Mutable as MU

import Hedgehog
import Hedgehog.Classes.Common
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Test that a 'Vector.Unboxed.MVector' instance obey several laws.
muvectorLaws :: (Eq a, MU.Unbox a, Show a) => Gen a -> Laws
muvectorLaws gen = Laws "Vector.Unboxed.MVector"
  [ ("New-Length", newLength gen)
  , ("Replicate-Length", replicateLength gen)
  , ("Slice-Length", sliceLength gen)
  , ("Grow-Length", growLength gen)

  , ("Write-Read", writeRead gen)
  , ("Set-Read", setRead gen)
  , ("Sliced-Set-Read", slicedSetRead gen)
  , ("Replicate-Read", replicateRead gen)

  , ("Slice-Overlaps", sliceOverlaps gen)
  , ("Slice-Copy", sliceCopy gen)
  , ("Slice-Move", sliceMove gen)

  , ("Write-Copy-Read", writeCopyRead gen)
  , ("Write-Move-Read", writeMoveRead gen)
  , ("Write-Grow-Read", writeGrowRead gen)
  , ("Sliced-Write-Copy-Read", slicedWriteCopyRead gen)
  , ("Sliced-Write-Move-Read", slicedWriteMoveRead gen)
  , ("Sliced-Write-Grow-Read", slicedWriteGrowRead gen)

  , ("Write-InitializeAround-Read", writeInitializeAroundRead gen)
  , ("Write-ClearAround-Read", writeClearAroundRead gen)
  , ("Write-SetAround-Read", writeSetAroundRead gen)
  , ("Write-WriteAround-Read", writeWriteAroundRead gen)
  , ("Write-CopyAround-Read", writeCopyAroundRead gen)
  , ("Write-MoveAround-Read", writeMoveAroundRead gen)

  , ("Write-InitializeBetween-Read", writeInitializeBetweenRead gen)
  , ("Write-ClearBetween-Read", writeClearBetweenRead gen)
  , ("Write-SetBetween-Read", writeSetBetweenRead gen)
  , ("Write-CopyBetween-Read", writeCopyBetweenRead gen)
  , ("Write-MoveBetween-Read", writeMoveBetweenRead gen)
  ]

genNonNegative :: Gen Int
genNonNegative = Gen.integral (Range.linear 0 1000)

genPositive :: Gen Int
genPositive = Gen.integral (Range.linear 1 1000)

-------------------------------------------------------------------------------
-- Length

newLength :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
newLength _ = property $ do
  len <- forAll genNonNegative
  (=== len) (runST $ MU.length <$> (MU.new len :: ST s (MU.MVector s a)))

replicateLength :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
replicateLength gen = property $ do
  a   <- forAll gen
  len <- forAll genNonNegative
  (=== len) (runST $ MU.length <$> MU.replicate len a)

sliceLength :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
sliceLength _ = property $ do
  ix     <- forAll genNonNegative
  subLen <- forAll genNonNegative
  excess <- forAll genPositive
  (=== subLen) (runST $ MU.length . MU.slice ix subLen <$> (MU.new (ix + subLen + excess) :: ST s (MU.MVector s a)))

growLength :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
growLength _ = property $ do
  len <- forAll genPositive
  by  <- forAll genPositive
  (=== len + by) $ runST $ do
    arr <- MU.new len :: ST s (MU.MVector s a)
    MU.length <$> MU.grow arr by

-------------------------------------------------------------------------------
-- Read

writeRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeRead gen = property $ do
  a      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  (=== a) $ runST $ do
    arr <- MU.new (ix + excess)
    MU.write arr ix a
    MU.read arr ix

setRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
setRead gen = property $ do
  a      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  (=== a) $ runST $ do
    arr <- MU.new (ix + excess)
    MU.set arr a
    MU.read arr ix

slicedSetRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
slicedSetRead gen = property $ do
  a      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  before <- forAll genNonNegative
  after  <- forAll genNonNegative
  (=== a) $ runST $ do
    arr <- newSlice before after (ix + excess)
    MU.set arr a
    MU.read arr ix

replicateRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
replicateRead gen = property $ do
  a      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  (=== a) $ runST $ do
    arr <- MU.replicate (ix + excess) a
    MU.read arr ix

-------------------------------------------------------------------------------
-- Overlaps

sliceOverlaps :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
sliceOverlaps _ = property $ do
  i  <- forAll genNonNegative
  ij <- forAll genNonNegative
  jk <- forAll genNonNegative
  kl <- forAll genNonNegative
  lm <- forAll genNonNegative
  let j = i + ij
      k = j + jk
      l = k + kl
      m = l + lm
  runST $ do
    arr <- MU.new (m + 1) :: ST s (MU.MVector s a)
    let slice1 = MU.slice i (k - i + 1) arr
        slice2 = MU.slice j (l - j + 1) arr
    pure $ assert $ MU.overlaps slice1 slice2

sliceCopy :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
sliceCopy gen = property $ do
  a      <- forAll gen
  i      <- forAll genNonNegative
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  ij     <- forAll genNonNegative
  jk     <- forAll genNonNegative
  let j = i + ix + excess + ij
      k = j + ix + excess + jk
  runST $ do
    arr <- MU.new k :: ST s (MU.MVector s a)
    let src = MU.slice i (ix + excess) arr
        dst = MU.slice j (ix + excess) arr
    if MU.overlaps src dst then pure success else do
      MU.write src ix a
      MU.copy dst src
      valSrc <- MU.read src ix
      valDst <- MU.read dst ix
      pure $ (valSrc, valDst) === (a, a)

sliceMove :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
sliceMove gen = property $ do
  a      <- forAll gen
  i      <- forAll genNonNegative
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  ij     <- forAll genNonNegative
  jk     <- forAll genNonNegative
  let j = i + ix + excess + ij
      k = j + ix + excess + jk
  (=== a) $ runST $ do
    arr <- MU.new k :: ST s (MU.MVector s a)
    let src = MU.slice i (ix + excess) arr
        dst = MU.slice j (ix + excess) arr
    MU.write src ix a
    MU.move dst src
    MU.read dst ix

-------------------------------------------------------------------------------
-- Write + copy/move/grow

writeCopyRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeCopyRead gen = property $ do
  a      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  (=== a) $ runST $ do
    src <- MU.new (ix + excess)
    MU.write src ix a
    dst <- MU.new (ix + excess)
    MU.copy dst src
    MU.clear src
    MU.read dst ix

writeMoveRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeMoveRead gen = property $ do
  a      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  (=== a) $ runST $ do
    src <- MU.new (ix + excess)
    MU.write src ix a
    dst <- MU.new (ix + excess)
    MU.move dst src
    MU.clear src
    MU.read dst ix

writeGrowRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeGrowRead gen = property $ do
  a      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  by     <- forAll genPositive
  (=== a) $ runST $ do
    src <- MU.new (ix + excess)
    MU.write src ix a
    dst <- MU.grow src by
    MU.clear src
    MU.read dst ix

slicedWriteCopyRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
slicedWriteCopyRead gen = property $ do
  a         <- forAll gen
  ix        <- forAll genNonNegative
  excess    <- forAll genPositive
  beforeSrc <- forAll genNonNegative
  afterSrc  <- forAll genNonNegative
  beforeDst <- forAll genNonNegative
  afterDst  <- forAll genNonNegative
  (=== a) $ runST $ do
    src <- newSlice beforeSrc afterSrc (ix + excess)
    MU.write src ix a
    dst <- newSlice beforeDst afterDst (ix + excess)
    MU.copy dst src
    MU.clear src
    MU.read dst ix

slicedWriteMoveRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
slicedWriteMoveRead gen = property $ do
  a         <- forAll gen
  ix        <- forAll genNonNegative
  excess    <- forAll genPositive
  beforeSrc <- forAll genNonNegative
  afterSrc  <- forAll genNonNegative
  beforeDst <- forAll genNonNegative
  afterDst  <- forAll genNonNegative
  (=== a) $ runST $ do
    src <- newSlice beforeSrc afterSrc (ix + excess)
    MU.write src ix a
    dst <- newSlice beforeDst afterDst (ix + excess)
    MU.move dst src
    MU.clear src
    MU.read dst ix

slicedWriteGrowRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
slicedWriteGrowRead gen = property $ do
  a         <- forAll gen
  ix        <- forAll genNonNegative
  excess    <- forAll genPositive
  by        <- forAll genPositive
  beforeSrc <- forAll genNonNegative
  afterSrc  <- forAll genNonNegative
  (=== a) $ runST $ do
    src <- newSlice beforeSrc afterSrc (ix + excess)
    MU.write src ix a
    dst <- MU.grow src by
    MU.clear src
    MU.read dst ix

-------------------------------------------------------------------------------
-- Write + overwrite around

writeInitializeAroundRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeInitializeAroundRead gen = property $ do
  a      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  (=== a) $ runST $ do
    arr <- MU.new (ix + excess)
    MU.write arr ix a
    when (ix > 0) $
      MU.basicInitialize (MU.slice 0 ix arr)
    when (excess > 1) $
      MU.basicInitialize (MU.slice (ix + 1) (excess - 1) arr)
    MU.read arr ix

writeClearAroundRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeClearAroundRead gen = property $ do
  a      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  (=== a) $ runST $ do
    arr <- MU.new (ix + excess)
    MU.write arr ix a
    when (ix > 0) $
      MU.clear (MU.slice 0 ix arr)
    when (excess > 1) $
      MU.clear (MU.slice (ix + 1) (excess - 1) arr)
    MU.read arr ix

writeSetAroundRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeSetAroundRead gen = property $ do
  a      <- forAll gen
  b      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  (=== a) $ runST $ do
    arr <- MU.new (ix + excess)
    MU.write arr ix a
    when (ix > 0) $
      MU.set (MU.slice 0 ix arr) b
    when (excess > 1) $
      MU.set (MU.slice (ix + 1) (excess - 1) arr) b
    MU.read arr ix

writeWriteAroundRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeWriteAroundRead gen = property $ do
  a      <- forAll gen
  b      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  (=== a) $ runST $ do
    arr <- MU.new (ix + excess)
    MU.write arr ix a
    when (ix > 0) $
      MU.write arr (ix - 1) b
    when (excess > 1) $
      MU.write arr (ix + 1) b
    MU.read arr ix

writeCopyAroundRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeCopyAroundRead gen = property $ do
  a      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  (=== a) $ runST $ do
    src <- MU.new (ix + excess)
    dst <- MU.new (ix + excess)
    MU.write dst ix a
    when (ix > 0) $
      MU.copy (MU.slice 0 ix dst) (MU.slice 0 ix src)
    when (excess > 1) $
      MU.copy (MU.slice (ix + 1) (excess - 1) dst) (MU.slice (ix + 1) (excess - 1) src)
    MU.read dst ix

writeMoveAroundRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeMoveAroundRead gen = property $ do
  a      <- forAll gen
  ix     <- forAll genNonNegative
  excess <- forAll genPositive
  (=== a) $ runST $ do
    src <- MU.new (ix + excess)
    dst <- MU.new (ix + excess)
    MU.write dst ix a
    when (ix > 0) $
      MU.move (MU.slice 0 ix dst) (MU.slice 0 ix src)
    when (excess > 1) $
      MU.move (MU.slice (ix + 1) (excess - 1) dst) (MU.slice (ix + 1) (excess - 1) src)
    MU.read dst ix

-------------------------------------------------------------------------------
-- Two writes + overwrite in between

writeInitializeBetweenRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeInitializeBetweenRead gen = property $ do
  a      <- forAll gen
  b      <- forAll gen
  ix     <- forAll genNonNegative
  dix    <- forAll genPositive
  excess <- forAll genPositive
  (=== (a, b)) $ runST $ do
    arr <- MU.new (ix + dix + excess)
    MU.write arr ix a
    MU.write arr (ix + dix) b
    MU.basicInitialize (MU.slice (ix + 1) (dix - 1) arr)
    (,) <$> MU.read arr ix <*> MU.read arr (ix + dix)

writeClearBetweenRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeClearBetweenRead gen = property $ do
  a      <- forAll gen
  b      <- forAll gen
  ix     <- forAll genNonNegative
  dix    <- forAll genPositive
  excess <- forAll genPositive
  (=== (a, b)) $ runST $ do
    arr <- MU.new (ix + dix + excess)
    MU.write arr ix a
    MU.write arr (ix + dix) b
    MU.clear (MU.slice (ix + 1) (dix - 1) arr)
    (,) <$> MU.read arr ix <*> MU.read arr (ix + dix)

writeSetBetweenRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeSetBetweenRead gen = property $ do
  a      <- forAll gen
  b      <- forAll gen
  c      <- forAll gen
  ix     <- forAll genNonNegative
  dix    <- forAll genPositive
  excess <- forAll genPositive
  (=== (a, b)) $ runST $ do
    arr <- MU.new (ix + dix + excess)
    MU.write arr ix a
    MU.write arr (ix + dix) b
    MU.set (MU.slice (ix + 1) (dix - 1) arr) c
    (,) <$> MU.read arr ix <*> MU.read arr (ix + dix)

writeCopyBetweenRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeCopyBetweenRead gen = property $ do
  a      <- forAll gen
  b      <- forAll gen
  ix     <- forAll genNonNegative
  dix    <- forAll genPositive
  excess <- forAll genPositive
  (=== (a, b)) $ runST $ do
    src <- MU.new (ix + dix + excess)
    dst <- MU.new (ix + dix + excess)
    MU.write dst ix a
    MU.write dst (ix + dix) b
    MU.copy (MU.slice (ix + 1) (dix - 1) dst) (MU.slice (ix + 1) (dix - 1) src)
    (,) <$> MU.read dst ix <*> MU.read dst (ix + dix)

writeMoveBetweenRead :: forall a. (Eq a, MU.Unbox a, Show a) => Gen a -> Property
writeMoveBetweenRead gen = property $ do
  a      <- forAll gen
  b      <- forAll gen
  ix     <- forAll genNonNegative
  dix    <- forAll genPositive
  excess <- forAll genPositive
  (=== (a, b)) $ runST $ do
    src <- MU.new (ix + dix + excess)
    dst <- MU.new (ix + dix + excess)
    MU.write dst ix a
    MU.write dst (ix + dix) b
    MU.move (MU.slice (ix + 1) (dix - 1) dst) (MU.slice (ix + 1) (dix - 1) src)
    (,) <$> MU.read dst ix <*> MU.read dst (ix + dix)

-------------------------------------------------------------------------------
-- Utils

newSlice :: MU.Unbox a => Int -> Int -> Int -> ST s (MU.MVector s a)
newSlice before after len = do
  arr <- MU.new (before + len + after)
  pure $ MU.slice before len arr

#endif
