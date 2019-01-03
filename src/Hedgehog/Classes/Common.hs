module Hedgehog.Classes.Common
  ( Laws(..)
  
  , hLessThan, hGreaterThan

  , genSmallList, genSmallNonEmptyList, genShowReadPrecedence, genSmallString, genQuadraticEquation, genSmallInteger

  , QuadraticEquation(..), runQuadraticEquation

  , hackReplace
  ) where

import Hedgehog
import GHC.Stack
import Hedgehog.Internal.Property
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data Laws = Laws
  { lawsTypeClass :: String
  , lawsProperties :: [(String, Property)]
  }

genSmallInteger :: Gen Integer
genSmallInteger = Gen.integral (Range.linear 0 20)

genSmallNonEmptyList :: Gen a -> Gen [a]
genSmallNonEmptyList gen = Gen.list (Range.linear 1 7) gen

genSmallList :: Gen a -> Gen [a]
genSmallList gen = Gen.list (Range.linear 0 6) gen

genSmallString :: Gen String
genSmallString = Gen.string (Range.linear 0 10) Gen.ascii

-- Haskell uses the operator precedences 0..9, the special function application
-- precedence 10 and the precedence 11 for function arguments. Both show and
-- read instances have to accept this range. According to the Haskell Language
-- Report, the output of derived show instances in precedence context 11 has to
-- be an atomic expression.
genShowReadPrecedence :: Gen Int
genShowReadPrecedence = Gen.element [0..11]

-- | Fails the test if the right argument is less than or equal to the left.
-- see https://github.com/hedgehogqa/haskell-hedgehog/pull/196 
hLessThan :: (MonadTest m, Ord a, Show a, HasCallStack) => a -> a -> m ()
hLessThan x y = do
  ok <- withFrozenCallStack $ eval (x < y)
  if ok
    then success
    else withFrozenCallStack $ failDiff x y

-- | Fails the test if the right argument is greater than or equal to the left.
-- see https://github.com/hedgehogqa/haskell-hedgehog/pull/196
hGreaterThan :: (MonadTest m, Ord a, Show a, HasCallStack) => a -> a -> m ()
hGreaterThan x y = do
  ok <- withFrozenCallStack $ eval (x > y)
  if ok
    then success
    else withFrozenCallStack $ failDiff x y

data QuadraticEquation = QuadraticEquation
  { _quadraticEquationQuadratic :: Integer
  , _quadraticEquationLinear :: Integer
  , _quadraticEquationConstant :: Integer
  }
  deriving (Eq)

-- This show instance does not actually provide a way
-- to create an equation. Instead, it makes it look
-- like a lambda.
instance Show QuadraticEquation where
  show (QuadraticEquation a b c) = "\\x -> " ++ show a ++ " * x ^ 2 + " ++ show b ++ " * x + " ++ show c

genQuadraticEquation :: Gen QuadraticEquation
genQuadraticEquation = do
  a <- Gen.integral (Range.linear 0 15)
  b <- Gen.integral (Range.linear 0 15)
  c <- Gen.integral (Range.linear 0 15)
  pure (QuadraticEquation a b c)

runQuadraticEquation :: QuadraticEquation -> Integer -> Integer
runQuadraticEquation (QuadraticEquation a b c) x = a * x ^ (2 :: Integer) + b * x + c

hackReplace :: (Functor f) => Gen b -> Gen (f a) -> Gen (f b)
hackReplace genb genfa = do
  b <- genb
  fmap2 (const b) genfa

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap