module Spec.Alternative (testAlternative) where

import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

testAlternative :: [(String, [Laws])]
testAlternative =
  [ ("[]", lawsList)
  , ("Maybe", lawsMaybe)
  ]

lawsList :: [Laws]
lawsList = [alternativeLaws (Gen.list (Range.linear 0 6))]

lawsMaybe :: [Laws]
lawsMaybe = [alternativeLaws Gen.maybe]

