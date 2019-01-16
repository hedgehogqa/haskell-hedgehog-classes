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
  ( ppDiscardCount
  , ppDoc
  , ppName
  , ppResult
  , ppShrinkCount
  , ppShrinkDiscard
  , ppTestCount
  , renderResult
  ) where

import Hedgehog.Range (Size)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.List as List
import Hedgehog.Internal.Report
  ( DiscardCount(..), TestCount(..), ShrinkCount(..), Markup(..), Report(..)
  , Result(..), FailureReport(..), renderDoc
  )
import qualified Text.PrettyPrint.Annotated.WL as WL
import Text.PrettyPrint.Annotated.WL ( (<+>) )
import qualified Hedgehog.Internal.Seed as Seed
import Text.PrettyPrint.Annotated.WL (Doc)

ppDoc :: Show x => x -> Doc a
ppDoc = WL.text . show

ppName :: Doc a
ppName = "<interactive>"

ppDiscardCount :: DiscardCount -> Doc a
ppDiscardCount = \case
  DiscardCount 1 -> "1 discard"
  DiscardCount n -> ppDoc n <+> "discards"

ppTestCount :: TestCount -> Doc a
ppTestCount = \case
  TestCount 1 -> "1 test"
  TestCount n -> ppDoc n <+> "tests"

ppShrinkCount :: ShrinkCount -> Doc a
ppShrinkCount = \case
  ShrinkCount 1 -> "1 shrink"
  ShrinkCount n -> ppDoc n <+> "shrinks"

ppShrinkDiscard :: ShrinkCount -> DiscardCount -> Doc Markup
ppShrinkDiscard s d = case (s, d) of
  (0, 0) -> ""
  (0, _) -> " and" <+> ppDiscardCount d
  (_, 0) -> " and" <+> ppShrinkCount s
  (_, _) -> "," <+> ppShrinkCount s <+> "and" <+> ppDiscardCount d

ppResult :: MonadIO m => Report Result -> m (Doc Markup)
ppResult (Report tests discards result) = case result of
  Failed failure -> do
    pfailure <- ppFailure failure
    pure . WL.vsep $
      [ icon FailedIcon '✗' . markup FailedHeader $
          ppName <+>
          "failed after" <+>
          ppTestCount tests <>
          ppShrinkDiscard (failureShrinks failure) discards <>
          "."
      , mempty
      , pfailure
      , mempty
      ] 
  
  GaveUp -> pure . icon GaveUpIcon '⚐' . WL.annotate GaveUpHeader $
    ppName <+>
    "gave up after" <+>
    ppDiscardCount discards <>
    ", passed" <+>
    ppTestCount tests <>
    "." 
  OK -> pure . icon SuccessIcon '✓' . WL.annotate SuccessHeader $
    ppName <+>
    "passed" <+>
    ppTestCount tests <>
    "."

icon :: Markup -> Char -> Doc Markup -> Doc Markup
icon m i x = WL.annotate m (WL.char i) <+> x

ppTextLines :: String -> [Doc Markup]
ppTextLines = fmap WL.text . List.lines

markup :: Markup -> Doc Markup -> Doc Markup
markup = WL.annotate

gutter :: Markup -> Doc Markup -> Doc Markup
gutter m x = markup m ">" <+> x

ppFailure :: MonadIO m => FailureReport -> m (Doc Markup)
ppFailure (FailureReport size seed _ _inputs0 mlocation0 msg _mdiff msgs0) = do
  msgs <- case mlocation0 of
    Nothing ->
      let msgs1 = msgs0 ++ (if null msg then [] else [msg])
          docs = concatMap ppTextLines msgs1
      in pure docs
    Just _location0 -> 
      let l = concatMap ppTextLines msgs0
      in pure l

  let with xs f = if null xs then [] else [f xs]

  pure . WL.indent 2 . WL.vsep . WL.punctuate WL.line $ concat
    [ with msgs WL.vsep
    , [ppReproduce size seed]
    ]

ppReproduce :: Size -> Seed.Seed -> Doc Markup
ppReproduce size seed = WL.vsep
  [ markup ReproduceHeader "This failure can be reproduced by running:"
  , gutter ReproduceGutter . markup ReproduceSource $
      "recheck" <+>
      WL.text (showsPrec 11 size "") <+>
      WL.text (showsPrec 11 seed "") <+>
      "<property>"
  ]

renderResult :: MonadIO m => Report Result -> m String
renderResult x = renderDoc Nothing =<< ppResult x

