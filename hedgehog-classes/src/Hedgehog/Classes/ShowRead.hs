{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.ShowRead (showReadLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Text.Read (readListDefault, readMaybe)
import Text.Show (showListWith)

-- | Tests the following 'Show' / 'Read' laws:
--
-- [__Partial Isomorphism: show/read__]: @'readMaybe' '.' 'show'@ ≡ @'Just'@
-- [__Partial Isomorphism: show/read with initial space__]: @'readMaybe' '.' (\" \" '++') '.' 'show'@ ≡ @'Just'@
-- [__Partial Isomorphism: showsPrec/readPrec__]: @(a,\"\") `elem` 'readsPrec' p ('showsPrec' p a \"\")@ ≡ @'True'@
-- [__Partial Isomorphism: showList/readList__]: @(as,\"\") `elem` 'readList' ('showList' as \"\")@ ≡ @'True'@
-- [__Partial Isomorphism: showListWith shows/readListDefault__]: @(as,\"\") `elem` 'readListDefault' ('showListWith' 'shows' as \"\")@ ≡ @'True'@
showReadLaws :: (Eq a, Read a, Show a) => Gen a -> Laws
showReadLaws gen = Laws "Show/Read"
  [ ("Partial Isomorphism: show/read", showReadPartialIsomorphism gen)
  , ("Partial Isomorphism: show/read with initial space", showReadSpacePartialIsomorphism gen)
  , ("Partial Isomorphism: showsPrec/readsPrec", showsPrecReadsPrecPartialIsomorphism gen)
  , ("Partial Isomorphism: showList/readList", showListReadListPartialIsomorphism gen)
  , ("Partial Isomorphism: showListWith shows/readListDefault", showListWithShowsReadListDefaultPartialIsomorphism gen)
  ]

showReadPartialIsomorphism :: forall a. (Eq a, Read a, Show a) => Gen a -> Property
showReadPartialIsomorphism gen = property $ do
  a <- forAll gen
  let lhs = readMaybe (show a)
  let rhs = Just a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Show/Read Partial Isomorphism", lawContextTcName = "Show/Read"
        , lawContextLawBody = "readMaybe . show" `congruency` "Just"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a;
            in lawWhere
              [ "readMaybe . show $ a" `congruency` "Just a, where"
              , "a = " ++ showA
              ]
        } 
  heqCtx lhs rhs ctx

showReadSpacePartialIsomorphism :: forall a. (Eq a, Read a, Show a) => Gen a -> Property
showReadSpacePartialIsomorphism gen = property $ do
  a <- forAll gen
  let lhs = readMaybe (" " ++ show a)
  let rhs = Just a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Show/Read Partial Isomorphism With Initial Space", lawContextTcName = "Show/Read"
        , lawContextLawBody = "readMaybe . (\" \" ++) . show" `congruency` "Just"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a;
            in lawWhere
              [ "readMaybe . (\" \" ++) . show $ a" `congruency` "Just a, where"
              , "a = " ++ showA
              ]
        }
  heqCtx lhs rhs ctx

showsPrecReadsPrecPartialIsomorphism :: forall a. (Eq a, Read a, Show a) => Gen a -> Property
showsPrecReadsPrecPartialIsomorphism gen = property $ do
  a <- forAll gen
  p <- forAll genShowReadPrecedence
  let lhs = (a,"") `elem` readsPrec p (showsPrec p a "")
  let rhs = True
  let ctx = contextualise $ LawContext
        { lawContextLawName = "ShowsPrec/ReadsPrec partial isomorphism", lawContextTcName = "Show/Read"
        , lawContextLawBody = "(a,\"\") `elem` readsPrec p (showsPrec p a \"\")" `congruency` "True"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a; showP = show p
            in lawWhere
              [ "(a,\"\") `elem` readsPrec p (showsPrec p a \"\")" `congruency` "True, where"
              , "a = " ++ showA
              , "p = " ++ showP
              ]
        }  
  heqCtx lhs rhs ctx

showListReadListPartialIsomorphism :: forall a. (Eq a, Read a, Show a) => Gen a -> Property
showListReadListPartialIsomorphism gen = property $ do
  as <- forAll $ genSmallList gen
  let lhs = (as,"") `elem` readList (showList as "")
  let rhs = True
  let ctx = contextualise $ LawContext
        { lawContextLawName = "ShowsList/ReadsList partial isomorphism", lawContextTcName = "Show/Read"
        , lawContextLawBody = "(as,\"\") `elem` readList (showList as \"\")" `congruency` "True"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showAS = show as
            in lawWhere
              [ "(as,\"\") `elem` readList (showList as \"\")" `congruency` "True, where"
              , "as = " ++ showAS
              ]
        }  
  heqCtx lhs rhs ctx
 
showListWithShowsReadListDefaultPartialIsomorphism :: forall a. (Eq a, Read a, Show a) => Gen a -> Property
showListWithShowsReadListDefaultPartialIsomorphism gen = property $ do
  as <- forAll $ genSmallList gen
  let lhs = (as,"") `elem` readListDefault (showListWith shows as "")
  let rhs = True
  let ctx = contextualise $ LawContext
        { lawContextLawName = "ShowListWith/ReadListDefault partial isomorphism", lawContextTcName = "Show/Read"
        , lawContextLawBody = "(as,\"\") `elem` readListDefault (showListWith shows as \"\")" `congruency` "True"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showAS = show as
            in lawWhere
              [ "(as,\"\") `elem` readListDefault (showListWith shows as \"\")" `congruency` "True, where"
              , "as = " ++ showAS 
              ]
        } 
  heqCtx lhs rhs ctx

