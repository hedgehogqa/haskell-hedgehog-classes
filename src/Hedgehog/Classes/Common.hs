{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Hedgehog.Classes.Common
  ( Laws(..)
  
  , hLessThan, hGreaterThan

  , genSmallList, genSmallNonEmptyList, genShowReadPrecedence, genSmallString, genSmallInteger, genSmallSum

  , QuadraticEquation(..), runQuadraticEquation, genQuadraticEquation
  , LinearEquation(..), runLinearEquation, genLinearEquation
  , LinearEquationM(..), runLinearEquationM, genLinearEquationM
  , LinearEquationTwo(..), runLinearEquationTwo, genLinearEquationTwo

  , ChooseFirst(..), genChooseFirst
  , ChooseSecond(..), genChooseSecond
  , LastNothing(..), genLastNothing
  , Bottom(..), genBottom

  , hackReplace

  , func1, func2, func3
  ) where

import Hedgehog
import GHC.Stack
import Data.Semigroup
import Hedgehog.Internal.Property
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.List as List

data Laws = Laws
  { lawsTypeClass :: String
  , lawsProperties :: [(String, Property)]
  }

genSmallSum :: Gen (Sum Integer)
genSmallSum = fmap Sum genSmallInteger

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

data LinearEquation = LinearEquation
  { _linearEquationLinear :: Integer
  , _linearEquationConstant :: Integer
  }
  deriving (Eq)

instance Show LinearEquation where
  showsPrec _ (LinearEquation a b) = shows a . showString " * x + " . shows b
  showList xs = appEndo
    $ mconcat
    $  [Endo (showChar '[')]
    ++ List.intersperse (Endo (showChar ',')) (map (Endo . showsPrec 0) xs)
    ++ [Endo (showChar ']')]

runLinearEquation :: LinearEquation -> Integer -> Integer
runLinearEquation (LinearEquation a b) x = a * x + b

genLinearEquation :: Gen LinearEquation
genLinearEquation = LinearEquation <$> genSmallInteger <*> genSmallInteger

data LinearEquationM m = LinearEquationM (m LinearEquation) (m LinearEquation)

deriving instance (forall x. Eq x => Eq (m x)) => Eq (LinearEquationM m)

instance (forall x. Show x => Show (m x)) => Show (LinearEquationM m) where
  show (LinearEquationM a b) = (\f -> f "")
    $ showString "\\x -> if odd x then "
    . showsPrec 0 a
    . showString " else "
    . showsPrec 0 b

runLinearEquationM :: Functor m => LinearEquationM m -> Integer -> m Integer
runLinearEquationM (LinearEquationM e1 e2) i = if odd i
  then fmap (flip runLinearEquation i) e1
  else fmap (flip runLinearEquation i) e2

genLinearEquationM :: Applicative m => Gen (LinearEquationM m)
genLinearEquationM = LinearEquationM <$> (pure <$> genLinearEquation) <*> (pure <$> genLinearEquation)

data LinearEquationTwo = LinearEquationTwo
  { _linearEquationTwoX :: Integer
  , _linearEquationTwoY :: Integer
  , _linearEquationTwoConstant :: Integer
  }

instance Show LinearEquationTwo where
  show (LinearEquationTwo x y c) = "\\x y -> " ++ show x ++ " * x + " ++ show y ++ " * y + " ++ show c

genLinearEquationTwo :: Gen LinearEquationTwo
genLinearEquationTwo = LinearEquationTwo <$> absGenInteger <*> absGenInteger <*> absGenInteger
  where
    absGenInteger = abs <$> genSmallInteger

runLinearEquationTwo :: LinearEquationTwo -> Integer -> Integer -> Integer
runLinearEquationTwo (LinearEquationTwo a b c) x y = a * x + b * y + c

hackReplace :: (Functor f) => Gen b -> Gen (f a) -> Gen (f b)
hackReplace genb genfa = do
  b <- genb
  fmap2 (const b) genfa

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

func1 :: Integer -> (Integer, Integer)
func1 i = (div (i + 5) 3, i * i - 2 * i + 1)

func2 :: (Integer, Integer) -> (Bool, Either Ordering Integer)
func2 (a,b) = (odd a, if even a then Left (compare a b) else Right (b + 2))

func3 :: Integer -> Sum Integer
func3 i = Sum (3 * i * i - 7 * i + 4)

data ChooseFirst = ChooseFirst
  deriving (Eq)

data ChooseSecond = ChooseSecond
  deriving (Eq)

data LastNothing = LastNothing
  deriving (Eq)

data Bottom a = BottomUndefined | BottomValue a
  deriving (Eq)

instance Show ChooseFirst where
  show ChooseFirst = "\\a b -> if even a then a else b"

instance Show ChooseSecond where
  show ChooseSecond = "\\a b -> if even b then a else b"

instance Show LastNothing where
  show LastNothing = "0"

instance Show a => Show (Bottom a) where
  show = \case
    BottomUndefined -> "undefined"
    BottomValue a   -> show a

genChooseFirst :: Gen ChooseFirst
genChooseFirst = pure ChooseFirst

genChooseSecond :: Gen ChooseSecond
genChooseSecond = pure ChooseSecond

genLastNothing :: Gen LastNothing
genLastNothing = pure LastNothing

genBottom :: Gen a -> Gen (Bottom a)
genBottom = fmap maybeToBottom . Gen.maybe

maybeToBottom :: Maybe a -> Bottom a
maybeToBottom = \case { Nothing -> BottomUndefined; Just a -> BottomValue a }
