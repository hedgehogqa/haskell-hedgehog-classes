{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Common.Compat
  ( readMaybe
  , eq
  , eq1
  , eq2

  , show1
  , show2

  , neq
  , neq1
  , neq2
  ) where

import Text.Read (readMaybe)

eq :: Eq a => a -> a -> Bool
eq = (==)

neq :: Eq a => a -> a -> Bool
neq = (/=)

eq1 :: (Eq a, forall x. Eq x => Eq (f x)) => f a -> f a -> Bool
eq1 = (==)

neq1 :: (Eq a, forall x. Eq x => Eq (f x)) => f a -> f a -> Bool
neq1 = (/=)

eq2 :: (Eq a, Eq b, forall x y. (Eq x, Eq y) => Eq (f x y)) => f a b -> f a b -> Bool
eq2 = (==)

neq2 :: (Eq a, Eq b, forall x y. (Eq x, Eq y) => Eq (f x y)) => f a b -> f a b -> Bool
neq2 = (/=)

show1 :: (Show a, forall x. (Show x) => Show (f x)) => f a -> String
show1 = Prelude.show

show2 :: (Show a, Show b, forall x y. (Show x, Show y) => Show (f x y)) => f a b -> String
show2 = Prelude.show
