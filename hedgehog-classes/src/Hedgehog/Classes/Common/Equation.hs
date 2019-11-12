{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Hedgehog.Classes.Common.Equation
  ( LinearEquation(..), runLinearEquation, genLinearEquation
  , LinearEquationTwo(..), runLinearEquationTwo, genLinearEquationTwo
  , LinearEquationM(..), runLinearEquationM, genLinearEquationM
  , QuadraticEquation(..), runQuadraticEquation, genQuadraticEquation
  , CubicEquation(..), runCubicEquation, genCubicEquation

#ifdef HAVE_COMONAD
  , LinearEquationW(..), runLinearEquationW, genLinearEquationW
#endif
  ) where

import Hedgehog
import Hedgehog.Classes.Common.Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.List as List

import Data.Monoid (Endo(..))

#ifdef HAVE_COMONAD
import Control.Comonad
#endif

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
#ifdef HAVE_COMONAD
data LinearEquationW w = LinearEquationW (w LinearEquation) (w LinearEquation)

deriving instance (forall x. Eq x => Eq (w x)) => Eq (LinearEquationW w)
instance (forall x. Show x => Show (w x)) => Show (LinearEquationW w) where
  show (LinearEquationW a b) = (\f -> f "")
    $ showString "\\x -> if odd x then "
    . showsPrec 0 a
    . showString " else "
    . showsPrec 0 b

runLinearEquationW :: Comonad w
  => LinearEquationW w -> w Integer -> Integer
runLinearEquationW (LinearEquationW e1 e2) (extract -> i) = if odd i
  then runLinearEquation (extract e1) i
  else runLinearEquation (extract e2) i

genLinearEquationW :: Comonad w
  => (forall x. Gen x -> Gen (w x))
  -> Gen (LinearEquationW w)
genLinearEquationW fgen = LinearEquationW
  <$> fgen genLinearEquation
  <*> fgen genLinearEquation
#endif

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
