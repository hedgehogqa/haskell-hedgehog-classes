{-# language
        DerivingStrategies
      , GeneralizedNewtypeDeriving
  #-}

{-# options_ghc -fno-warn-orphans #-}

module Spec.Comonad
  ( testComonad
  ) where

import Data.List.NonEmpty
import qualified Control.Applicative as App (liftA2)
import Control.Comonad
import Control.Comonad.Store hiding (store)
import Data.Functor.Identity (Identity(..))
import Hedgehog
import Hedgehog.Classes
import Prelude hiding (either)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

testComonad :: [(String, [Laws])]
testComonad =
  [ ("Identity", [comonadLaws identity])
  , ("NonEmpty", [comonadLaws nonempty])
  , ("(,) e", [comonadLaws tup])
  , ("StoreT Integer Identity", [comonadLaws store])
  ]

store :: MonadGen m => m a -> m (StoreT Integer Identity a)
store gen = do
  a <- gen
  pure $ StoreT (Identity (const a)) 20

instance (Comonad w, Show s, Show a) => Show (StoreT s w a) where
  show (StoreT wf s) = show $ "StoreT { s = " ++ show s ++ ", extract stuff = " ++ show (extract wf s) ++ "}"

instance (Comonad w, Eq a) => Eq (StoreT s w a) where
  StoreT wf s == StoreT wf' s' = extract wf s == extract wf' s'

identity :: MonadGen m => m a -> m (Identity a)
identity = fmap Identity

nonempty :: MonadGen m => m a -> m (NonEmpty a)
nonempty gen = App.liftA2 (:|) gen (list gen)

tup :: MonadGen m => m a -> m (Integer, a)
tup gen = (,)
  <$> Gen.integral (Range.linear 20 50)
  <*> gen

list :: MonadGen m => m a -> m [a]
list = Gen.list $ Range.linear 0 6
