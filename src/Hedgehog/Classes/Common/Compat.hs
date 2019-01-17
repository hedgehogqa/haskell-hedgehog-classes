{-# LANGUAGE CPP #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

module Hedgehog.Classes.Common.Compat
  ( readMaybe
  , eq 
  , eq1
  , eq2

  , neq
  , neq1
  , neq2
  ) where

#if HAVE_QUANTIFIED_CONSTRAINTS == 0
import qualified Data.Functor.Classes as C
#endif

import Text.Read (readMaybe)

eq :: Eq a => a -> a -> Bool
eq = (==)

neq :: Eq a => a -> a -> Bool
neq = (/=)

#if HAVE_QUANTIFIED_CONSTRAINTS
eq1 :: (Eq a, forall x. Eq x => Eq (f x)) => f a -> f a -> Bool
eq1 = (==)
#else
eq1 :: (C.Eq1 f, Eq a) => f a -> f a -> Bool
eq1 = C.Eq1
#endif

#if HAVE_QUANTIFIED_CONSTRAINTS
neq1 :: (Eq a, forall x. Eq x => Eq (f x)) => f a -> f a -> Bool
neq1 = (/=)
#else
neq1 :: (C.Eq1 f, Eq a) => f a -> f a -> Bool
neq1 x y = not $ C.Eq1 x y 
#endif

#if HAVE_QUANTIFIED_CONSTRAINTS
eq2 :: (Eq a, Eq b, forall x y. (Eq x, Eq y) => Eq (f x y)) => f a b -> f a b -> Bool
eq2 = (==)
#else
eq2 :: (C.Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = C.eq2
#endif

#if HAVE_QUANTIFIED_CONSTRAINTS
neq2 :: (Eq a, Eq b, forall x y. (Eq x, Eq y) => Eq (f x y)) => f a b -> f a b -> Bool
neq2 = (/=)
#else
neq2 :: (C.Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
neq2 x y = not $ C.eq2 x y
#endif

