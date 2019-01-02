module Hedgehog.Classes.Common
  ( Laws(..)
  
  , hLessThan, hGreaterThan

  , genSmallList
  ) where

import Hedgehog
import GHC.Stack
import Hedgehog.Internal.Property
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data Laws = Laws
  { lawsTypeClass :: String
  , lawsProperties :: [(String, Property)]
  }

genSmallList :: Gen a -> Gen [a]
genSmallList gen = Gen.list (Range.linear 0 6) gen

-- | Fails the test if the right argument is less than or equal to the left.
-- see https://github.com/hedgehogqa/haskell-hedgehog/pull/196 
hLessThan :: (MonadTest m, Ord a, Show a, HasCallStack) => a -> a -> m ()
hLessThan x y = do
  ok <- withFrozenCallStack $ eval (x < y)
  if ok
    then success
    else withFrozenCallStack $ failDiff x y

-- | Fails the test if the right argument is greater than or equal to the left.
-- see https://github.com/hedgehogqa/haskell-hedgehog/pull/196
hGreaterThan :: (MonadTest m, Ord a, Show a, HasCallStack) => a -> a -> m ()
hGreaterThan x y = do
  ok <- withFrozenCallStack $ eval (x > y)
  if ok
    then success
    else withFrozenCallStack $ failDiff x y