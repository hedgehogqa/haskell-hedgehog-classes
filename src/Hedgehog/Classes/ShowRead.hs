{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.ShowRead (showReadLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Text.Read (readListDefault, readMaybe)
import Text.Show (showListWith)

showReadLaws :: (Eq a, Read a, Show a) => Gen a -> Laws
showReadLaws gen = Laws "Show/Read"
  [ ("Partial Isomorphism: show/read", showReadPartialIsomorphism gen)
  , ("Partial Isomorphism: show/read with initial space", showReadSpacePartialIsomorphism gen)
  , ("Partial Isomorphism: showsPrec/readsPrec", showsPrecReadsPrecPartialIsomorphism gen)
  , ("Partial Isomorphism: showList/readList", showListReadListPartialIsomorphism gen)
  , ("Partial Isomorphism: showListWith shows / readListDefault", showListWithShowsReadListDefaultPartialIsomorphism gen)
  ]

showReadPartialIsomorphism :: forall a. (Eq a, Read a, Show a) => Gen a -> Property
showReadPartialIsomorphism gen = property $ do
  a <- forAll gen
  readMaybe (show a) === Just a

showReadSpacePartialIsomorphism :: forall a. (Eq a, Read a, Show a) => Gen a -> Property
showReadSpacePartialIsomorphism gen = property $ do
  a <- forAll gen
  readMaybe (" " ++ show a) === Just a

showsPrecReadsPrecPartialIsomorphism :: forall a. (Eq a, Read a, Show a) => Gen a -> Property
showsPrecReadsPrecPartialIsomorphism gen = property $ do
  a <- forAll gen
  p <- forAll genShowReadPrecedence
  ((a,"") `elem` readsPrec p (showsPrec p a "")) === True

showListReadListPartialIsomorphism :: forall a. (Eq a, Read a, Show a) => Gen a -> Property
showListReadListPartialIsomorphism gen = property $ do
  as <- forAll $ genSmallList gen
  ((as,"") `elem` readList (showList as "")) === True

showListWithShowsReadListDefaultPartialIsomorphism :: forall a. (Eq a, Read a, Show a) => Gen a -> Property
showListWithShowsReadListDefaultPartialIsomorphism gen = property $ do
  as <- forAll $ genSmallList gen
  ((as,"") `elem` readListDefault (showListWith shows as "")) === True

