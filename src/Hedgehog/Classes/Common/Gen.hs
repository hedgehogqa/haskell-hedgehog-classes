{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Common.Gen
  ( genSmallList
  , genSmallNonEmptyList
  , genShowReadPrecedence
  , genSmallString
  , genSmallInteger
  , genSmallSum
  , genCompose
  , genSetInteger

  -- * Used for 'Hedgehog.Classes.ixLaws' 
  , genTuple
  , genTuple3
  , genInRange
  , genValidRange
  ) where

import Data.Ix (Ix(..))
import Hedgehog
import Data.Functor.Compose
import qualified Data.Set as S
import Data.Semigroup
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genSmallSum :: Gen (Sum Integer)
genSmallSum = fmap Sum genSmallInteger

genSmallInteger :: Gen Integer
genSmallInteger = Gen.integral (Range.linear 0 20)

genSmallNonEmptyList :: Gen a -> Gen [a]
genSmallNonEmptyList gen = Gen.list (Range.linear 1 7) gen

genSmallList :: Gen a -> Gen [a]
genSmallList gen = Gen.list (Range.linear 0 6) gen

genSmallString :: Gen String
genSmallString = Gen.string (Range.linear 0 6) Gen.ascii

-- Haskell uses the operator precedences 0..9, the special function application
-- precedence 10 and the precedence 11 for function arguments. Both show and
-- read instances have to accept this range. According to the Haskell Language
-- Report, the output of derived show instances in precedence context 11 has to
-- be an atomic expression.
genShowReadPrecedence :: Gen Int
genShowReadPrecedence = Gen.element [0..11]

genCompose :: forall f g a. Gen a -> (forall x. Gen x -> Gen (f x)) -> (forall x. Gen x -> Gen (g x)) -> Gen (Compose f g a)
genCompose gen fgen ggen = Compose <$> fgen (ggen gen) 

genTuple :: Gen a -> Gen b -> Gen (a,b)
genTuple a b = (,) <$> a <*> b

genTuple3 :: Gen a -> Gen b -> Gen c -> Gen (a, b, c)
genTuple3 gena genb genc = do
  a <- gena
  b <- genb
  c <- genc
  pure (a, b, c)

genValidRange :: Ix a => Gen a -> Gen (a, a)
genValidRange gen = do
  Gen.filter (\(l,u) -> l <= u) (genTuple gen gen)

genInRange :: (Ix a) => Gen a -> Gen (a, a, a)
genInRange gen = do
  Gen.filter (\(l,u,i) -> inRange (l,u) i) (genTuple3 gen gen gen)
 
genSetInteger :: Gen (S.Set Integer)
genSetInteger = do
  xs <- sequence $ fmap (const genSmallInteger) [1..10 :: Integer]
  pure $ foldMap S.singleton xs

