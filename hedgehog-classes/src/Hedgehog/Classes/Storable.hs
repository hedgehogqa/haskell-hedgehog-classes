{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Storable (storableLaws) where

import Hedgehog
import Hedgehog.Classes.Common
import Hedgehog.Internal.Gen (sample)

import qualified Data.List as List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import GHC.Ptr (Ptr(..), plusPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)

-- | Tests the following 'Storable' laws:
--
-- [__Set-Get__]: @'pokeElemOff' ptr ix a '>>' 'peekElemOff' ptr ix@ ≡ @'pure' a@
-- [__Get-Set__]: @'peekElemOff' ptr ix '>>=' 'pokeElemOff' ptr ix@ ≡ @'pure' ()@ (Putting back what you got out has no effect)
-- [__List Conversion Roundtrips__]: Mallocing a list and then reconstructing it gives you the same list
-- [__PeekElemOff/Peek__]: @'peekElemOff' a i@ ≡ @'peek' ('plusPtr' a (i '*' 'sizeOf' 'undefined'))@
-- [__PokeElemOff/Poke__]: @'pokeElemOff' a i x@ ≡ @'poke' ('plusPtr' a (i '*' 'sizeOf' 'undefined')) x@
-- [__PeekByteOff/Peek__]: @'peekByteOff' a i@ ≡ @'peek' ('plusPtr' a i)@
-- [__PokeByteOff/Peek__]: @'pokeByteOff' a i x@ ≡ @'poke' ('plusPtr' a i) x@
storableLaws :: (Eq a, Show a, Storable a) => Gen a -> Laws
storableLaws gen = Laws "Storable"
  [ ("Set-Get (you get back what you put in)", storableSetGet gen)
  , ("Get-Set (putting back what you got out has no effect)", storableGetSet gen)
  , ("List Conversion Roundtrips", storableList gen)
  , ("peekElemOff a i ≡ peek (plusPtr a (i * sizeOf undefined))", storablePeekElem gen)
  , ("pokeElemOff a i x ≡ poke (plusPtr a (i * sizeOf undefined)) x ≡ id ", storablePokeElem gen)
  , ("peekByteOff a i ≡ peek (plusPtr a i)", storablePeekByte gen)
  , ("pokeByteOff a i x ≡ poke (plusPtr a i) x ≡ id ", storablePokeByte gen)
  ]

genArray :: forall a. (Storable a) => Gen a -> Int -> IO (Ptr a)
genArray gen len = do
  let go ix xs = if ix == len
        then pure xs
        else do
          x <- sample gen
          go (ix + 1) (x : xs)
  as <- go 0 []
  newArray as

storablePeekElem :: forall a. (Eq a, Show a, Storable a) => Gen a -> Property
storablePeekElem gen = property $ do
  as <- forAll $ genSmallNonEmptyList gen
  let len = List.length as
  ix <- forAll $ Gen.int (Range.linear 0 (len - 1))
  unsafePerformIO $ do
    addr <- genArray gen len
    x <- peekElemOff addr ix
    y <- peek (addr `plusPtr` (ix * sizeOf (undefined :: a)))
    free addr
    pure (x === y)

storablePokeElem :: forall a. (Eq a, Show a, Storable a) => Gen a -> Property
storablePokeElem gen = property $ do
  as <- forAll $ genSmallNonEmptyList gen
  x <- forAll gen
  let len = List.length as
  ix <- forAll $ Gen.int (Range.linear 0 (len - 1))
  unsafePerformIO $ do
    addr <- genArray gen len
    pokeElemOff addr ix x
    u <- peekElemOff addr ix
    poke (addr `plusPtr` (ix * sizeOf x)) x
    v <- peekElemOff addr ix
    free addr
    pure (u === v)

storablePeekByte :: forall a. (Eq a, Show a, Storable a) => Gen a -> Property
storablePeekByte gen = property $ do
  as <- forAll $ genSmallNonEmptyList gen
  let len = List.length as
  off <- forAll $ Gen.int (Range.linear 0 (len - 1))
  unsafePerformIO $ do
    addr <- genArray gen len
    x :: a <- peekByteOff addr off
    y :: a <- peek (addr `plusPtr` off)
    free addr
    pure (x === y)

storablePokeByte :: forall a. (Eq a, Show a, Storable a) => Gen a -> Property
storablePokeByte gen = property $ do
  as <- forAll $ genSmallNonEmptyList gen
  x <- forAll gen
  let len = List.length as
  off <- forAll $ Gen.int (Range.linear 0 (len - 1))
  unsafePerformIO $ do
    addr <- genArray gen len
    pokeByteOff addr off x
    u :: a <- peekByteOff addr off
    poke (addr `plusPtr` off) x
    v :: a <- peekByteOff addr off
    free addr
    pure (u === v)

storableSetGet :: forall a. (Eq a, Show a, Storable a) => Gen a -> Property
storableSetGet gen = property $ do
  a <- forAll gen
  len <- forAll $ Gen.int (Range.linear 1 20)
  ix <- forAll $ Gen.int (Range.linear 0 (len - 1))
  unsafePerformIO $ do
    ptr <- genArray gen len
    pokeElemOff ptr ix a
    a' <- peekElemOff ptr ix
    free ptr
    pure (a === a')

storableGetSet :: forall a. (Eq a, Show a, Storable a) => Gen a -> Property
storableGetSet gen = property $ do
  as <- forAll $ genSmallNonEmptyList gen
  let len = List.length as
  ix <- forAll $ Gen.int (Range.linear 0 (len - 1))
  unsafePerformIO $ do
    ptrA <- newArray as
    ptrB <- genArray gen len
    copyArray ptrB ptrA len
    a <- peekElemOff ptrA ix
    pokeElemOff ptrA ix a
    res <- arrayEq ptrA ptrB len
    free ptrA
    free ptrB
    pure (res === True)

storableList :: forall a. (Eq a, Show a, Storable a) => Gen a -> Property
storableList gen = property $ do
  as <- forAll $ genSmallNonEmptyList gen 
  unsafePerformIO $ do
    let len = List.length as
    ptr <- newArray as
    let rebuild :: Int -> IO [a]
        rebuild ix = if ix < len
          then (:) <$> peekElemOff ptr ix <*> rebuild (ix + 1)
          else pure []
    asNew <- rebuild 0
    free ptr
    pure (as === asNew)

arrayEq :: forall a. (Eq a, Storable a) => Ptr a -> Ptr a -> Int -> IO Bool
arrayEq ptrA ptrB len = go 0 where
  go i = if i < len
    then do
      a <- peekElemOff ptrA i
      b <- peekElemOff ptrB i
      if a == b
        then go (i + 1)
        else pure False
    else pure True
