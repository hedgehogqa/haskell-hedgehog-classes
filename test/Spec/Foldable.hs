module Spec.Foldable (testFoldable) where

import Hedgehog
import Hedgehog.Classes

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.List as List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

testFoldable :: [(String, [Laws])]
testFoldable =
  [ ("Set", listSet)
--  , ("BadList", listBadList)
  ]

listSet :: [Laws]
listSet = [foldableLaws genSet]

genSet :: Gen a -> Gen (Set a)
genSet gen = do
  x <- gen
  pure (Set.singleton x)

listBadList :: [Laws]
listBadList = [foldableLaws genBadList]

genBadList :: Gen a -> Gen (BadList a)
genBadList gen = BadList <$> Gen.list (Range.linear 0 20) gen

newtype BadList a = BadList [a]
  deriving (Eq, Show)

instance Foldable BadList where
  foldMap f (BadList x) = foldMap f x
  foldl' = List.foldl
