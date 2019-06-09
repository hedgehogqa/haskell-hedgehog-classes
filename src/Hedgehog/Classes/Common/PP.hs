{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

-- | Reverse-engineered hedgehog internals that don't print out source locations.
module Hedgehog.Classes.Common.PP
  ( ppResult
  , renderResult
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Hedgehog.Internal.Report hiding (ppResult, renderResult)
import Text.PrettyPrint.Annotated.WL (Doc)
import qualified Hedgehog.Internal.Report as R

renderResult :: MonadIO m
  => Report Result
  -> m String
renderResult x = renderDoc Nothing =<< ppResult x

ppResult :: MonadIO m
  => Report Result
  -> m (Doc Markup)
ppResult r@(Report tests discards coverage status) = case status of
  Failed (FailureReport size seed shrinks _mcoverage annots _mspan msg _mdiff footnotes) ->
    let failure = Failed $ FailureReport size seed shrinks Nothing annots Nothing msg Nothing footnotes
    in R.ppResult Nothing (Report tests discards coverage failure)
  _ -> R.ppResult Nothing r
