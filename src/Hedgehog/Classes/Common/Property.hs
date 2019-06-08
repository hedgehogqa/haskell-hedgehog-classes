{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | This module exports hedgehog comparison tests
--   that don't contain CallStack information, since this would
--   expose library internals in error messages.
module Hedgehog.Classes.Common.Property
  ( hLessThan, hGreaterThan
  , heq, heq1, heq2
  , heqCtx, heqCtx1, heqCtx2
  , hneq, hneq1, hneq2
  , hneqCtx, hneqCtx1, hneqCtx2
  , bar
  , Context(..)
  ) where

import Control.Exception (SomeException(..), displayException)
import Data.Typeable (typeOf)
import GHC.Stack
import Hedgehog.Classes.Common.Compat
import Hedgehog.Internal.Exception (tryEvaluate)
import Hedgehog.Internal.Property (MonadTest, liftTest, mkTest, success, Failure(..))
import Text.Show.Pretty (ppShow)
import qualified Data.Char as Char
import qualified Data.List as List

bar :: String
bar = "━━━"

bar5 :: String
bar5 = "━━━━━━━━━━━━━━━"

evalNoSrc :: (MonadTest m, HasCallStack) => a -> m a
evalNoSrc x = either (withFrozenCallStack failExceptionNoSrc) pure (tryEvaluate x)

failWithNoSrc :: (MonadTest m, HasCallStack) => String -> m a
failWithNoSrc msg = do
  liftTest $ mkTest (Left $ Failure Nothing msg Nothing, mempty)

failExceptionNoSrc :: (MonadTest m, HasCallStack) => SomeException -> m a
failExceptionNoSrc (SomeException x) = withFrozenCallStack $
  failWithNoSrc $ unlines
    [ bar ++ " Exception: " ++ show (typeOf x) ++ " " ++ bar
      , List.dropWhileEnd Char.isSpace (displayException x)
    ]

-- | You can provide a 'Context' to 'heqCtx','heqCtx1','heqCtx2','hneqCtx','hneqCtx1',or 'hneqCtx2'. The 'Context' is used to provide useful error messages in the event of a failure.
data Context = NoContext | Context String

contextToString :: Context -> String
contextToString = \case
  NoContext -> "No Context provided."
  Context ctx -> bar ++ " Context " ++ bar ++ "\n" ++ ctx ++ "\n" ++ bar5

failContext::
  ( MonadTest m, HasCallStack
  , Show a, Show b
  ) => a -> b -> Context -> m ()
failContext _x _y ctx = withFrozenCallStack $
  failWithNoSrc $ contextToString ctx

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

infix 4 `hneq`

-- | Passes the test if the given arguments are not equal. Otherwise fails
--   with the given 'Context'.
hneqCtx ::
  ( MonadTest m
  , HasCallStack
  , Eq a
  , Show a
  ) => a -> a -> Context -> m ()
hneqCtx x y ctx = do
  ok <- withFrozenCallStack $ evalNoSrc (x `neq` y)
  if ok
    then success
    else withFrozenCallStack $ failContext x y ctx

-- | Passes the test if the given arguments are not equal. Otherwise fails
--   with 'NoContext'.
hneq ::
  ( MonadTest m
  , HasCallStack
  , Eq a
  , Show a
  ) => a -> a -> m ()
hneq x y = hneqCtx x y NoContext

infix 4 `heq`

-- | Passes the test if the given arguments are equal. Otherwise fails
--   with the given 'Context'.
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

-- | Passes the test if the given arguments are equal. Otherwise fails
--   with 'NoContext'.
heq ::
    ( MonadTest m
    , HasCallStack
    , Eq a
    , Show a
    ) => a -> a -> m ()
heq x y = heqCtx x y NoContext

infix 4 `heq1`

-- | Passes the test if the given arguments are not equal. Otherwise fails
--   with the given 'Context'.
hneqCtx1 ::
     ( MonadTest m
     , HasCallStack
     , Eq a
     , Show a
     , forall x. Eq x => Eq (f x)
     , forall x. Show x => Show (f x)
     ) => f a -> f a -> Context -> m ()
hneqCtx1 x y ctx = do
  ok <- withFrozenCallStack $ evalNoSrc (x `neq1` y)
  if ok
    then success
    else withFrozenCallStack $ failContext x y ctx

-- | Passes the test if the given arguments are not equal. Otherwise fails
--   with 'NoContext'.
hneq1 ::
     ( MonadTest m
     , HasCallStack
     , Eq a
     , Show a
     , forall x. Eq x => Eq (f x)
     , forall x. Show x => Show (f x)
     ) => f a -> f a -> m ()
hneq1 x y = hneqCtx1 x y NoContext

-- | Passes the test if the given arguments are equal. Otherwise fails
--   with the given 'Context'.
heqCtx1 ::
     ( MonadTest m
     , HasCallStack
     , Eq a
     , Show a
     , forall x. Eq x => Eq (f x)
     , forall x. Show x => Show (f x)
     ) => f a -> f a -> Context -> m ()
heqCtx1 x y ctx = do
  ok <- withFrozenCallStack $ evalNoSrc (x `eq1` y)
  if ok
    then success
    else withFrozenCallStack $ failContext x y ctx

-- | Passes the test if the given arguments are equal. Otherwise fails
--   with 'NoContext'.
heq1 ::
     ( MonadTest m
     , HasCallStack
     , Eq a
     , Show a
     , forall x. Eq x => Eq (f x)
     , forall x. Show x => Show (f x)
     ) => f a -> f a -> m ()
heq1 x y = heqCtx1 x y NoContext

infix 4 `heq2`

-- | Passes the test if the given arguments are equal. Otherwise fails
--   with the given 'Context'.
heqCtx2 ::
     ( MonadTest m
     , HasCallStack
     , Eq a
     , Eq b
     , Show a
     , Show b
     , forall x y. (Eq x, Eq y) => Eq (f x y)
     , forall x y. (Show x, Show y) => Show (f x y)
     ) => f a b -> f a b -> Context -> m ()
heqCtx2 x y ctx = do
  ok <- withFrozenCallStack $ evalNoSrc (x `eq2` y)
  if ok
    then success
    else withFrozenCallStack $ failContext x y ctx

-- | Passes the test if the given arguments are equal. Otherwise fails
--   with 'NoContext'.
heq2 ::
     ( MonadTest m
     , HasCallStack
     , Eq a
     , Eq b
     , Show a
     , Show b
     , forall x y. (Eq x, Eq y) => Eq (f x y)
     , forall x y. (Show x, Show y) => Show (f x y)
     ) => f a b -> f a b -> m ()
heq2 x y = heqCtx2 x y NoContext

infix 4 `hneq2`

-- | Passes the test if the given arguments are not equal. Otherwise fails
--   with the given 'Context'.
hneqCtx2 ::
     ( MonadTest m
     , HasCallStack
     , Eq a
     , Eq b
     , Show a
     , Show b
     , forall x y. (Eq x, Eq y) => Eq (f x y)
     , forall x y. (Show x, Show y) => Show (f x y)
     ) => f a b -> f a b -> Context -> m ()
hneqCtx2 x y ctx = do
  ok <- withFrozenCallStack $ evalNoSrc (x `neq2` y)
  if ok
    then success
    else withFrozenCallStack $ failContext x y ctx

-- | Passes the test if the given arguments are not equal. Otherwise fails
--   with 'NoContext'.
hneq2 ::
     ( MonadTest m
     , HasCallStack
     , Eq a
     , Eq b
     , Show a
     , Show b
     , forall x y. (Eq x, Eq y) => Eq (f x y)
     , forall x y. (Show x, Show y) => Show (f x y)
     ) => f a b -> f a b -> m ()
hneq2 x y = hneqCtx2 x y NoContext
