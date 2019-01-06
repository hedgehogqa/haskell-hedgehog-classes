{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Common
  ( Laws(..)
  
  , hLessThan, hGreaterThan

  , genSmallList, genSmallNonEmptyList, genShowReadPrecedence, genSmallString, genSmallInteger, genSmallSum

  , QuadraticEquation(..), runQuadraticEquation, genQuadraticEquation
  , LinearEquation(..), runLinearEquation, genLinearEquation
  , LinearEquationM(..), runLinearEquationM, genLinearEquationM
  , LinearEquationTwo(..), runLinearEquationTwo, genLinearEquationTwo
  , CubicEquation(..), runCubicEquation, genCubicEquation

  , Bottom(..), genBottom

  , Triple(..), reverseTriple, genTriple

  , apTrans, toSpecialApplicative

  , func1, func2, func3, func4, func5, func6
  
  , genCompose, genTuple, genSetInteger
  ) where

import Data.Tuple (swap)
import Hedgehog
import GHC.Stack
import Data.Functor.Classes (Eq1(..), Show1(..))
import Data.Functor.Compose
import qualified Data.Set as S
import qualified Control.Monad.Trans.Writer.Lazy as WL
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

data CubicEquation = CubicEquation
  { _cubicEquationCubic :: Integer
  , _cubicEquationQuadratic :: Integer
  , _cubicEquationLinear :: Integer
  , _cubicEquationConstant :: Integer
  }

instance Show CubicEquation where
  show (CubicEquation x y z c) = "\\x -> " ++ show x ++ " * x ^ 3 + " ++ show y ++ " * x ^ 2 + " ++ show z ++ " * x + " ++ show c

genCubicEquation :: Gen CubicEquation
genCubicEquation = CubicEquation <$> genSmallInteger <*> genSmallInteger <*> genSmallInteger <*> genSmallInteger

runCubicEquation :: CubicEquation -> Integer -> Integer -> Integer -> Integer
runCubicEquation (CubicEquation a b c d) x y z = a * x + b * y + c * z + d

func1 :: Integer -> (Integer, Integer)
func1 i = (div (i + 5) 3, i * i - 2 * i + 1)

func2 :: (Integer, Integer) -> (Bool, Either Ordering Integer)
func2 (a,b) = (odd a, if even a then Left (compare a b) else Right (b + 2))

func3 :: Integer -> Sum Integer
func3 i = Sum (3 * i * i - 7 * i + 4)

func4 :: Integer -> Compose Triple (WL.Writer (S.Set Integer)) Integer
func4 i = Compose $ Triple
  (WL.writer (i * i, S.singleton (i * 7 + 5)))
  (WL.writer (i + 2, S.singleton (i * i + 3)))
  (WL.writer (i * 7, S.singleton 4))

func5 :: Integer -> Triple Integer
func5 i = Triple (i + 2) (i * 3) (i * i)

func6 :: Integer -> Triple Integer
func6 i = Triple (i * i * i) (4 * i - 7) (i * i * i)

reverseTriple :: Triple a -> Triple a
reverseTriple (Triple a b c) = Triple c b a

data Triple a = Triple a a a
  deriving (Show, Eq)

instance Functor Triple where
  fmap f (Triple a b c) = Triple (f a) (f b) (f c)

instance Applicative Triple where
  pure a = Triple a a a
  Triple f g h <*> Triple a b c = Triple (f a) (g b) (h c)

instance Foldable Triple where
  foldMap f (Triple a b c) = f a <> f b <> f c

instance Traversable Triple where
  traverse f (Triple a b c) = Triple <$> f a <*> f b <*> f c

tripleLiftEq :: (a -> b -> Bool) -> Triple a -> Triple b -> Bool
tripleLiftEq p (Triple a1 b1 c1) (Triple a2 b2 c2) =
  p a1 a2 && p b1 b2 && p c1 c2

tripleLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Triple a -> ShowS
tripleLiftShowsPrec elemShowsPrec _ p (Triple a b c) = showParen (p > 10)
  $ showString "Triple "
  . elemShowsPrec 11 a
  . showString " "
  . elemShowsPrec 11 b
  . showString " "
  . elemShowsPrec 11 c

instance Eq1 Triple where
  liftEq = tripleLiftEq

instance Show1 Triple where
  liftShowsPrec = tripleLiftShowsPrec

genTriple :: Gen a -> Gen (Triple a)
genTriple gen = Triple <$> gen <*> gen <*> gen
  
data Bottom a = BottomUndefined | BottomValue a
  deriving (Eq)

instance Show a => Show (Bottom a) where
  show = \case
    BottomUndefined -> "undefined"
    BottomValue a   -> show a

genBottom :: Gen a -> Gen (Bottom a)
genBottom = fmap maybeToBottom . Gen.maybe

maybeToBottom :: Maybe a -> Bottom a
maybeToBottom = \case { Nothing -> BottomUndefined; Just a -> BottomValue a }

-- Reverse the list and accumulate the writers. We
-- cannot use Sum or Product or else it won't actually
-- be a valid applicative transformation.
apTrans ::
     Compose Triple (WL.Writer (S.Set Integer)) a
  -> Compose (WL.Writer (S.Set Integer)) Triple a
apTrans (Compose xs) = Compose (sequenceA (reverseTriple xs))

toSpecialApplicative ::
     Compose Triple ((,) (S.Set Integer)) Integer
  -> Compose Triple (WL.Writer (S.Set Integer)) Integer
toSpecialApplicative (Compose (Triple a b c)) =
  Compose (Triple (WL.writer (swap a)) (WL.writer (swap b)) (WL.writer (swap c)))

genCompose :: forall f g a. Gen a -> (forall x. Gen x -> Gen (f x)) -> (forall x. Gen x -> Gen (g x)) -> Gen (Compose f g a)
genCompose gen fgen ggen = Compose <$> fgen (ggen gen) 

genTuple :: Gen a -> Gen b -> Gen (a,b)
genTuple a b = (,) <$> a <*> b

genSetInteger :: Gen (S.Set Integer)
genSetInteger = do
  xs <- sequence $ fmap (const genSmallInteger) [1..10 :: Integer]
  pure $ foldMap S.singleton xs
