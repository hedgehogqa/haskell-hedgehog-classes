module Spec.Traversable (testTraversable) where

import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

testTraversable :: [(String, [Laws])]
testTraversable =
  [ ("[]", lawsList)
  ]

lawsList :: [Laws]
lawsList = [traversableLaws (Gen.list (Range.linear 0 6))]
