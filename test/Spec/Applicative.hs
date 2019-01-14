module Spec.Applicative (testApplicative) where

import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))

import Hedgehog
import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Prelude hiding (either)

testApplicative :: [(String, [Laws])]
testApplicative =
  [ ("[]", lawsList)
  , ("Maybe", lawsMaybe)
  , ("Either e", lawsEither)
  , ("Compose", lawsCompose)
  ]

lawsList :: [Laws]
lawsList = [applicativeLaws (Gen.list (Range.linear 0 6))]

lawsMaybe :: [Laws]
lawsMaybe = [applicativeLaws Gen.maybe]

lawsEither :: [Laws]
lawsEither = [applicativeLaws eitherInteger]

lawsCompose :: [Laws]
lawsCompose = [applicativeLaws genCompose]

genCompose :: Gen a -> Gen (Compose Identity Identity a)
genCompose = fmap (Compose . Identity . Identity)

eitherInteger :: MonadGen m => m a -> m (Either Integer a)
eitherInteger = either (Gen.integral (Range.linear 0 20))

either :: MonadGen m => m e -> m a -> m (Either e a)
either genE genA =
  Gen.sized $ \n ->
    Gen.frequency [
        (2, Left <$> genE)
      , (1 + fromIntegral n, Right <$> genA)
      ]
