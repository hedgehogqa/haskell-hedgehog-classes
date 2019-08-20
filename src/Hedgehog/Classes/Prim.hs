{-# language LambdaCase #-}
{-# language UnboxedTuples #-}
{-# language TypeApplications #-}
{-# language MagicHash #-}
{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}

module Hedgehog.Classes.Prim (primLaws) where

import Control.Monad (when)
import Foreign.Marshal.Alloc
import GHC.Exts

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Primitive
import Data.Primitive
import Data.Primitive.Ptr

import Hedgehog
import Hedgehog.Classes.Common
import Hedgehog.Internal.Gen (sample)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Tests the following 'Prim' laws:
--
-- [__ByteArray Set-Get__]: @'primitive_' ('writeByteArray#' ba# ix# x) '*>' 'primitive' ('readByteArray#' ba# ix#)@ ≡ @'pure' x@
-- [__ByteArray Get-Set__]: @'primitive' ('readByteArray#' ba# ix#) '>>=' \x -> 'primitive_' ('writeByteArray#' ba# ix# x)@ ≡ @'pure' ()@
-- [__ByteArray Set-Set__]: @'primitive_' ('writeByteArray#' ba# ix# x) *> 'primitive_' ('writeByteArray#' ba# ix# x)@ ≡ @'primitive_' ('writeByteArray#' ba# ix# x)@
-- [__ByteArray Set Range__]: The behavior of 'setByteArray#' matches the default implementation
-- [__ByteArray List Conversion Roundtrips__]: Turning a list into a 'PrimArray' and back gives you the same list
-- [__Addr Set-Get__]: @'primitive_' ('writeOffAddr#' addr# ix# x) '*>' 'primitive' ('readOffAddr#' addr# ix#)@ ≡ @'pure' x@
-- [__Addr Get-Set__]: @'primitive' ('readOffAddr#' addr# ix#) '>>=' \x -> 'primitive_' ('writeOffAddr#' addr# ix# x)@ ≡ @'pure' ()@
-- [__Addr Set-Set__]: @'primitive_' ('writeOffAddr#' addr# ix# x) '*>' 'primitive_' ('writeOffAddr#' addr# ix# x)@ ≡ @'primitive_' ('writeOffAddr#' addr# ix# x)@
-- [__Addr Set Range__]: The behavior of 'setOffAddr#' matches the default implementation
-- [__Addr List Conversion Roundtrips__]: Mallocing a list and then reconstructing it gives you the same list
primLaws :: (Prim a, Eq a, Show a) => Gen a -> Laws
primLaws gen = Laws "Prim"
  [ ("ByteArray Set-Get (you get back what you put in)", primSetGetByteArray gen)
  , ("ByteArray Get-Set (putting back what you got out has no effect)", primGetSetByteArray gen)
  , ("ByteArray Set-Set (putting twice is the same as putting once)", primSetSetByteArray gen)
  , ("ByteArray Set Range", primSetByteArray gen)
  , ("ByteArray List Conversion Roundtrips", primListRoundtripByteArray gen)
  , ("Addr Set-Get (you get back what you put in)", primSetGetAddr gen)
  , ("Addr Get-Set (putting back what you got out has no effect)", primGetSetAddr gen)
  , ("Addr Set-Set (putting twice is the same as putting once)", primSetSetAddr gen)
  , ("Addr Set Range", primSetRangeAddr gen)
  , ("Addr List Conversion Roundtrips", primListRoundtripAddr gen)
  ]

genSmallArrayLen :: Gen Int
genSmallArrayLen = Gen.integral (Range.linear 1 10)

genMutPrimArray :: Prim a => Gen a -> Int -> IO (MutablePrimArray (PrimState IO) a)
genMutPrimArray gen len = do
  marr <- newPrimArray len
  let go :: Int -> IO ()
      go !ix = when (ix < len) $ do
        writePrimArray marr ix =<< sample gen
        go (ix + 1)
  go 0
  pure marr

genPrimArray :: forall a. Prim a => Gen a -> Int -> IO (PrimArray a)
genPrimArray gen len = unsafeFreezePrimArray =<< genMutPrimArray gen len

-- | Setting an element and getting it back should give back the same element
primSetGetByteArray :: (Eq a, Show a, Prim a) => Gen a -> Property
primSetGetByteArray gen = property $ do
  len <- forAll genSmallArrayLen
  ix <- forAll $ Gen.int (Range.linear 0 (len - 1))
  el <- forAll gen
  el' <- liftIO $ do
    marr <- genMutPrimArray gen len
    writePrimArray marr ix el
    readPrimArray marr ix
  el === el'

-- | Getting an element and putting it back should not change the array
primGetSetByteArray :: (Eq a, Show a, Prim a) => Gen a -> Property
primGetSetByteArray gen = property $ do
  len <- forAll genSmallArrayLen
  ix <- forAll $ Gen.int (Range.linear 0 (len - 1))
  (arr1, arr2) <- liftIO $ do
    arr1 <- genPrimArray gen len
    marr2 <- newPrimArray len
    copyPrimArray marr2 0 arr1 0 len
    writePrimArray marr2 ix =<< readPrimArray marr2 ix
    arr2 <- unsafeFreezePrimArray marr2
    pure (arr1, arr2)
  arr1 === arr2

-- | Setting and element once and twice should result in the same array (setting
-- should be idempotent)
primSetSetByteArray :: (Eq a, Show a, Prim a) => Gen a -> Property
primSetSetByteArray gen = property $ do
  len <- forAll genSmallArrayLen
  ix <- forAll $ Gen.int (Range.linear 0 (len - 1))
  el <- forAll gen
  (arr1, arr2) <- liftIO $ do
    marr1 <- genMutPrimArray gen len
    writePrimArray marr1 ix el
    marr2 <- newPrimArray len
    copyMutablePrimArray marr2 0 marr1 0 len
    arr1 <- unsafeFreezePrimArray marr1
    writePrimArray marr2 ix el
    arr2 <- unsafeFreezePrimArray marr2
    pure (arr1, arr2)
  arr1 === arr2

-- | Setting a range should match the default implementation
primSetByteArray :: (Eq a, Show a, Prim a) => Gen a -> Property
primSetByteArray gen = property $ do
  len <- forAll genSmallArrayLen
  (low, high) <- fmap order $ (,)
    <$> forAll (Gen.int (Range.linear 0 (len - 1)))
    <*> forAll (Gen.int (Range.linear 0 (len - 1)))
  el <- forAll gen
  (arr2, arr3) <- liftIO $ do
    arr1 <- genPrimArray gen len
    marr2 <- newPrimArray len
    copyPrimArray marr2 0 arr1 0 len
    marr3 <- newPrimArray len
    copyPrimArray marr3 0 arr1 0 len
    setPrimArray marr2 low (high - low) el
    arr2 <- unsafeFreezePrimArray marr2
    defaultSetPrimArray marr3 low (high - low) el
    arr3 <- unsafeFreezePrimArray marr3
    pure (arr2, arr3)
  arr2 === arr3
  where
    order (x, y) = if x < y then (x, y) else (y, x)

defaultSetPrimArray :: (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -> Int -> Int -> a -> m ()
defaultSetPrimArray (MutablePrimArray marr#) (I# off#) (I# len#) x = primitive_ (go off#)
  where
    end# = off# +# len#
    go !ix# s# = if isTrue# (ix# >=# end#)
      then s#
      else case writeByteArray# marr# ix# x s# of
        s2# -> go (ix# +# 1#) s2#

-- | @'toList' . 'fromList'@ should result in the same list
primListRoundtripByteArray :: forall a. (Eq a, Show a, Prim a) => Gen a -> Property
primListRoundtripByteArray gen = property $ do
  xs <- forAll $ genSmallNonEmptyList gen
  xs === toList (fromList xs :: PrimArray a)

withBytes :: forall a b. Prim a => Int -> (Ptr a -> IO b) -> IO b
withBytes len h = do
  p <- mallocBytes (len * sizeOf (undefined :: a))
  h p <* free p

ptrToPrimArray :: forall a. Prim a => Ptr a -> Int -> IO (PrimArray a)
ptrToPrimArray p len = do
  marr <- newPrimArray len
  copyPtrToMutablePrimArray marr 0 p len
  unsafeFreezePrimArray marr

-- | Setting an element and getting it back should give back the same element
primSetGetAddr :: forall a. (Eq a, Show a, Prim a) => Gen a -> Property
primSetGetAddr gen = property $ do
  len <- forAll genSmallArrayLen
  ix <- forAll $ Gen.int (Range.linear 0 (len - 1))
  el <- forAll gen
  el' <- liftIO $ withBytes len $ \p -> do
    writeOffPtr p ix el
    readOffPtr p ix
  el === el'

-- | Getting an element and putting it back should not change the array
primGetSetAddr :: (Eq a, Show a, Prim a) => Gen a -> Property
primGetSetAddr gen = property $ do
  len <- forAll genSmallArrayLen
  ix <- forAll $ Gen.int (Range.linear 0 (len - 1))
  (arr1, arr2) <- liftIO $ do
    arr1 <- genPrimArray gen len
    arr2 <- withBytes len $ \p -> do
      copyPrimArrayToPtr p arr1 0 len
      writeOffPtr p ix =<< readOffPtr p ix
      ptrToPrimArray p len
    pure (arr1, arr2)
  arr1 === arr2

-- | Setting and element once and twice should result in the same array (setting
-- should be idempotent)
primSetSetAddr :: (Eq a, Show a, Prim a) => Gen a -> Property
primSetSetAddr gen = property $ do
  len <- forAll genSmallArrayLen
  ix <- forAll $ Gen.int (Range.linear 0 (len - 1))
  el <- forAll gen
  (arr2, arr3) <- liftIO $ do
    arr1 <- genPrimArray gen len
    withBytes len $ \p1 -> do
      copyPrimArrayToPtr p1 arr1 0 len
      writeOffPtr p1 ix el
      arr2 <- ptrToPrimArray p1 len
      withBytes len $ \p2 -> do
        copyPrimArrayToPtr p2 arr2 0 len
        writeOffPtr p2 ix el
        arr3 <- ptrToPrimArray p2 len
        pure (arr2, arr3)
  arr2 === arr3

-- | Setting a range should match the default implementation
primSetRangeAddr :: (Eq a, Show a, Prim a) => Gen a -> Property
primSetRangeAddr gen = property $ do
  len <- forAll genSmallArrayLen
  (low, high) <- fmap order $ (,)
    <$> forAll (Gen.int (Range.linear 0 (len - 1)))
    <*> forAll (Gen.int (Range.linear 0 (len - 1)))
  el <- forAll gen
  (arr2, arr3) <- liftIO $ do
    withBytes len $ \p1 -> do
      withBytes len $ \p2 -> do
        arr1 <- genPrimArray gen len
        copyPrimArrayToPtr p1 arr1 0 len
        copyPrimArrayToPtr p2 arr1 0 len
        setOffPtr p1 low (high - low) el
        arr2 <- ptrToPrimArray p1 len
        defaultSetOffAddr p2 low (high - low) el
        arr3 <- ptrToPrimArray p2 len
        pure (arr2, arr3)
  arr2 === arr3
  where
    order (x, y) = if x < y then (x, y) else (y, x)
    setOffPtr (Ptr addr#) (I# off#) (I# len#) x =
      primitive_ (setOffAddr# addr# off# len# x)

defaultSetOffAddr :: (Prim a, PrimMonad m) => Ptr a -> Int -> Int -> a -> m ()
defaultSetOffAddr (Ptr addr#) (I# off#) (I# len#) x = primitive_ (go off#)
  where
    end# = off# +# len#
    go !ix# s# = if isTrue# (ix# >=# end#)
      then s#
      else case writeOffAddr# addr# ix# x s# of
        s2# -> go (ix# +# 1#) s2#

-- | Mallocing an array, emptying a list into the array, and then rebuilding the
-- list from that array should produce the original list.
primListRoundtripAddr :: forall a. (Eq a, Show a, Prim a) => Gen a -> Property
primListRoundtripAddr gen = property $ do
  xs <- forAll $ genSmallList gen
  let len = length xs
  xs' <- liftIO $ withBytes len $ \p -> do
    let listToPtr :: Int -> [a] -> IO ()
        listToPtr !ix = \case
          [] -> pure ()
          (y:ys) -> writeOffPtr p ix y *> listToPtr (ix + 1) ys
    let ptrToList :: Int -> IO [a]
        ptrToList !ix =
          if ix >= len
            then pure []
            else (:) <$> readOffPtr p ix <*> ptrToList (ix + 1)
    listToPtr 0 xs
    ptrToList 0
  xs === xs'

