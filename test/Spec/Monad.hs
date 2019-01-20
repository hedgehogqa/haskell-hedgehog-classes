module Spec.Monad (testMonad) where

import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))

import Hedgehog
import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Prelude hiding (either)

testMonad :: [(String, [Laws])]
testMonad =
  [ ("[]", lawsList)
  , ("Maybe", lawsMaybe)
  , ("Either e", lawsEither)
  ]

lawsList :: [Laws]
lawsList = [monadLaws (Gen.list (Range.linear 0 6))]

lawsMaybe :: [Laws]
lawsMaybe = [monadLaws Gen.maybe]

lawsEither :: [Laws]
lawsEither = [monadLaws eitherInteger]

eitherInteger :: MonadGen m => m a -> m (Either Integer a)
eitherInteger = either (Gen.integral (Range.linear 0 20))

either :: MonadGen m => m e -> m a -> m (Either e a)
either genE genA =
  Gen.sized $ \n ->
    Gen.frequency [
        (2, Left <$> genE)
      , (1 + fromIntegral n, Right <$> genA)
      ]
