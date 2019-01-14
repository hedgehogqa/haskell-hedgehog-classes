module Spec.Bitraversable (testBitraversable) where

import Data.Functor.Const (Const(..))
import Hedgehog
import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import Prelude hiding (either, const)

testBitraversable :: [(String, [Laws])]
testBitraversable =
  [ ("Either", lawsEither)
  , ("Const", lawsConst) 
  ]

lawsEither :: [Laws]
lawsEither = [bitraversableLaws either]

lawsConst :: [Laws]
lawsConst = [bitraversableLaws const]

const :: MonadGen m => m a -> m b -> m (Const a b)
const genA _genB = fmap Const genA

either :: MonadGen m => m e -> m a -> m (Either e a)
either genE genA =
  Gen.sized $ \n ->
    Gen.frequency [
        (2, Left <$> genE)
      , (1 + fromIntegral n, Right <$> genA)
      ]