module Spec.Bifunctor (testBifunctor) where

import Data.Functor.Const (Const(..))
import Hedgehog
import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import Prelude hiding (either, const)

testBifunctor :: [(String, [Laws])]
testBifunctor =
  [ ("Either", lawsEither)
  , ("Const", lawsConst) 
  ]

lawsEither :: [Laws]
lawsEither = [bifunctorLaws either]

lawsConst :: [Laws]
lawsConst = [bifunctorLaws const]

const :: MonadGen m => m a -> m b -> m (Const a b)
const genA _genB = fmap Const genA

either :: MonadGen m => m e -> m a -> m (Either e a)
either genE genA =
  Gen.sized $ \n ->
    Gen.frequency [
        (2, Left <$> genE)
      , (1 + fromIntegral n, Right <$> genA)
      ]