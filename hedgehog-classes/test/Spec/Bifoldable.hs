module Spec.Bifoldable (testBifoldable, testBifoldableFunctor) where

import Data.Functor.Const (Const(..))
import Hedgehog
import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import Prelude hiding (either, const)

testBifoldable :: [(String, [Laws])]
testBifoldable =
  [ ("Either", lawsEither)
  , ("Const", lawsConst) 
  ]

testBifoldableFunctor :: [(String, [Laws])]
testBifoldableFunctor =
  [ ("Either", functorLawsEither)
  , ("Const", functorLawsConst)
  ]

lawsConst, functorLawsConst :: [Laws]
lawsConst = [bifoldableLaws const]
functorLawsConst = [bifoldableFunctorLaws const]

const :: MonadGen m => m a -> m b -> m (Const a b)
const genA _genB = fmap Const genA

lawsEither, functorLawsEither :: [Laws]
lawsEither = [bifoldableLaws either]
functorLawsEither = [bifoldableFunctorLaws either]

either :: MonadGen m => m e -> m a -> m (Either e a)
either genE genA =
  Gen.sized $ \n ->
    Gen.frequency [
        (2, Left <$> genE)
      , (1 + fromIntegral n, Right <$> genA)
      ]
