{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Show (showLaws) where

import Hedgehog
import Hedgehog.Classes.Common

showLaws :: (Show a) => Gen a -> Laws
showLaws gen = Laws "Show"
  [ ("Show", showShowsPrecZero gen)
  , ("Equivariance: showsPrec", equivarianceShowsPrec gen)
  , ("Equivariance: showList", equivarianceShowList gen)
  ]

showShowsPrecZero :: forall a. (Show a) => Gen a -> Property
showShowsPrecZero gen = property $ do
  a <- forAll gen
  show a === showsPrec 0 a ""

equivarianceShowsPrec :: forall a. (Show a) => Gen a -> Property
equivarianceShowsPrec gen = property $ do
  p <- forAll genShowReadPrecedence
  a <- forAll gen
  r <- forAll genSmallString
  s <- forAll genSmallString
  showsPrec p a r ++ s === showsPrec p a (r ++ s)

equivarianceShowList :: forall a. (Show a) => Gen a -> Property
equivarianceShowList gen = property $ do
  as <- forAll $ genSmallList gen
  r <- forAll genSmallString
  s <- forAll genSmallString
  showList as r ++ s === showList as (r ++ s)
