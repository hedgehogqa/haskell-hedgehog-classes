module Spec.Foldable (testFoldable) where

import Hedgehog
import Hedgehog.Classes

import Data.Set (Set)
import qualified Data.Set as Set

testFoldable :: [(String, [Laws])]
testFoldable =
  [ ("Set", listSet)
  ]

listSet :: [Laws]
listSet = [foldableLaws genSet]

genSet :: Gen a -> Gen (Set a)
genSet gen = do
  x <- gen
  pure (Set.singleton x)
