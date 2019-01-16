{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

-- | This module exports hedgehog comparison tests
--   that don't contain CallStack information, since this would
--   expose library internals in error messages.
module Hedgehog.Classes.Common.Property
  ( hLessThan, hGreaterThan
  , heq, heq1, heq2
  , heqCtx, heqCtx1, heqCtx2
  , bar
  , Context(..)
  ) where

import Control.Exception (SomeException(..), displayException)
import Data.Typeable (typeOf)
import qualified Data.List as List
import qualified Data.Char as Char
import Hedgehog.Internal.Exception (tryEvaluate)
import GHC.Stack
import Hedgehog.Internal.Property
  ( MonadTest, liftTest, mkTest, success, Failure(..), Log(..)
  )
import Hedgehog.Classes.Common.Compat
import Text.Show.Pretty (ppShow)

#if HAVE_QUANTIFIED_CONSTRAINTS == 0
import qualified Data.Functor.Classes as C
#endif

bar :: String
bar = "━━━"

evalNoSrc :: (MonadTest m, HasCallStack) => a -> m a
evalNoSrc x = either (withFrozenCallStack failExceptionNoSrc) pure (tryEvaluate x)

failWithNoSrc :: (MonadTest m, HasCallStack) => String -> m a
failWithNoSrc msg = do
  liftTest $ mkTest (Left $ Failure Nothing "" Nothing, [Footnote msg])
 
failExceptionNoSrc :: (MonadTest m, HasCallStack) => SomeException -> m a
failExceptionNoSrc (SomeException x) = withFrozenCallStack $
  failWithNoSrc $ unlines
    [ bar ++ " Exception: " ++ show (typeOf x) ++ " " ++ bar
      , List.dropWhileEnd Char.isSpace (displayException x)
    ]

data Context = NoContext | Context String

contextToString :: Context -> String
contextToString = \case
  NoContext -> ""
  Context ctx -> unlines [bar ++ " Context " ++ bar, ctx]

failContext::
  ( MonadTest m, HasCallStack
  , Show a, Show b
  ) => a -> b -> Context -> m ()
failContext _x _y ctx = withFrozenCallStack $
  failWithNoSrc $ unlines
    [ --bar ++ " Not Equal " ++ bar
--    , "  " ++ ppShow x
--    , "  " ++ ppShow y
      contextToString ctx 
    ]

-- | Fails the test if the right argument is less than or equal to the left.
-- see https://github.com/hedgehogqa/haskell-hedgehog/pull/196 
hLessThan :: (MonadTest m, Ord a, Show a, HasCallStack) => a -> a -> m ()
hLessThan x y = do
  ok <- withFrozenCallStack $ evalNoSrc (x < y)
  if ok
    then success
    else withFrozenCallStack $ failWithNoSrc $ unlines
      [ bar ++ "Not Less Than " ++ bar
      , ppShow x ++ " is not less than " ++ ppShow y
      ]

-- | Fails the test if the right argument is greater than or equal to the left.
-- see https://github.com/hedgehogqa/haskell-hedgehog/pull/196
hGreaterThan :: (MonadTest m, Ord a, Show a, HasCallStack) => a -> a -> m ()
hGreaterThan x y = do
  ok <- withFrozenCallStack $ evalNoSrc (x > y)
  if ok
    then success
    else withFrozenCallStack $ failWithNoSrc $ unlines
      [ bar ++ "Not Greater Than " ++ bar
      , ppShow x ++ " is not greater than " ++ ppShow y
      ]

infix 4 `heq`

heqCtx ::
    ( MonadTest m
    , HasCallStack
    , Eq a
    , Show a
    ) => a -> a -> Context -> m ()
heqCtx x y ctx = do
  ok <- withFrozenCallStack $ evalNoSrc (x `eq` y)
  if ok
    then success
    else withFrozenCallStack $ failContext x y ctx

heq ::
    ( MonadTest m
    , HasCallStack
    , Eq a
    , Show a
    ) => a -> a -> m ()
heq x y = heqCtx x y NoContext

infix 4 `heq1`

heqCtx1 ::
     ( MonadTest m
     , HasCallStack
     , Eq a
     , Show a
#if HAVE_QUANTIFIED_CONSTRAINTS
     , forall x. Eq x => Eq (f x)
     , forall x. Show x => Show (f x)
#else
     , C.Eq1 f
#endif 
     ) => f a -> f a -> Context -> m ()
heqCtx1 x y ctx = do
  ok <- withFrozenCallStack $ evalNoSrc (x `eq1` y)
  if ok
    then success
    else withFrozenCallStack $ failContext x y ctx

heq1 ::
     ( MonadTest m
     , HasCallStack
     , Eq a
     , Show a
#if HAVE_QUANTIFIED_CONSTRAINTS
     , forall x. Eq x => Eq (f x)
     , forall x. Show x => Show (f x)
#else
     , C.Eq1 f
#endif 
     ) => f a -> f a -> m ()
heq1 x y = heqCtx1 x y NoContext

infix 4 `heq2`

heqCtx2 ::
     ( MonadTest m
     , HasCallStack
     , Eq a
     , Eq b
     , Show a
     , Show b
#if HAVE_QUANTIFIED_CONSTRAINTS
     , forall x y. (Eq x, Eq y) => Eq (f x y)
     , forall x y. (Show x, Show y) => Show (f x y)
#else
     , C.Eq2 f
#endif 
     ) => f a b -> f a b -> Context -> m ()
heqCtx2 x y ctx = do
  ok <- withFrozenCallStack $ evalNoSrc (x `eq2` y)
  if ok
    then success
    else withFrozenCallStack $ failContext x y ctx

heq2 ::
     ( MonadTest m
     , HasCallStack
     , Eq a
     , Eq b
     , Show a
     , Show b
#if HAVE_QUANTIFIED_CONSTRAINTS
     , forall x y. (Eq x, Eq y) => Eq (f x y)
     , forall x y. (Show x, Show y) => Show (f x y)
#else
     , C.Eq2 f
#endif 
     ) => f a b -> f a b -> m ()
heq2 x y = heqCtx2 x y NoContext

