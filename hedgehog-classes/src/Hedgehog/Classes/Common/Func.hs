module Hedgehog.Classes.Common.Func
  ( func1
  , func2
  , func3
  , func4
  , func5
  , func6

  , Triple(..), reverseTriple, genTriple
  ) where

import Hedgehog
import Data.Functor.Classes (Eq1(..), Show1(..))
import Data.Functor.Compose
import qualified Data.Set as S
import qualified Control.Monad.Trans.Writer.Lazy as WL
import Data.Semigroup

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
  