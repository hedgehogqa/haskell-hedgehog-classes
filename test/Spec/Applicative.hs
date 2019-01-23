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
--  , ("Bin", lawsBin)
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

{-
data Bin a = Leaf | Node (Bin a) a (Bin a)
  deriving (Eq, Show)

instance Functor Bin where
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Applicative Bin where
  pure x = Node Leaf x Leaf
  Leaf <*> _ = Leaf
  _ <*> Leaf = Leaf
  Node fl fx fr <*> Node l x r = Node (fl <*> l) (fx x) (fr <*> r)

genBin' :: Gen a -> Gen (Bin a)
genBin' gen = do
  x <- gen
  pure $ Node (Node Leaf x (Node Leaf x Leaf)) x (Node (Node Leaf x Leaf) x Leaf)

genBin :: Gen a -> Gen (Bin a)
genBin gen = Gen.frequency
  [ (1, pure Leaf)
  , (6, genBin' gen)
  ]

lawsBin :: [Laws]
lawsBin = [applicativeLaws genBin]
-}