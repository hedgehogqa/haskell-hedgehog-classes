{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 805
#define HAVE_QUANTIFIED_CONSTRAINTS 1
{-# LANGUAGE QuantifiedConstraints #-}
#else
#define HAVE_QUANTIFIED_CONSTRAINTS 0
#endif

module Hedgehog.Classes.Common.Compat
  ( readMaybe
  , eq 
  , eq1
  , eq2
  ) where

#if HAVE_QUANTIFIED_CONSTRAINTS == 0
import qualified Data.Functor.Classes as C
#endif

import Text.Read (readMaybe)

eq :: Eq a => a -> a -> Bool
eq = (==)

#if HAVE_QUANTIFIED_CONSTRAINTS
eq1 :: (Eq a, forall x. Eq x => Eq (f x)) => f a -> f a -> Bool
eq1 = (==)
#else
eq1 :: (C.Eq1 f, Eq a) => f a -> f a -> Bool
eq1 = C.Eq1
#endif

#if HAVE_QUANTIFIED_CONSTRAINTS
eq2 :: (Eq a, Eq b, forall x y. (Eq x, Eq y) => Eq (f x y)) => f a b -> f a b -> Bool
eq2 = (==)
#else
eq2 :: (C.Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = C.eq2
#endif


