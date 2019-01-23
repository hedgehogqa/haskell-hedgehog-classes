{-# LANGUAGE CPP #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

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
eq1 = C.eq1
#endif

#if HAVE_QUANTIFIED_CONSTRAINTS
neq1 :: (Eq a, forall x. Eq x => Eq (f x)) => f a -> f a -> Bool
neq1 = (/=)
#else
neq1 :: (C.Eq1 f, Eq a) => f a -> f a -> Bool
neq1 x y = not $ C.eq1 x y 
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

#if HAVE_QUANTIFIED_CONSTRAINTS
show1 :: (Show a, forall x. (Show x) => Show (f x)) => f a -> String
show1 = Prelude.show
#else
show1 :: (C.Show1 f, Show a) => f a -> String
show1 x = C.showsPrec1 0 x ""
#endif

#if HAVE_QUANTIFIED_CONSTRAINTS
show2 :: (Show a, Show b, forall x y. (Show x, Show y) => Show (f x y)) => f a b -> String
show2 = Prelude.show
#else
show2 :: (C.Show2 f, Show a, Show b) => f a b -> String
show2 x = C.showsPrec2 0 x ""
#endif
