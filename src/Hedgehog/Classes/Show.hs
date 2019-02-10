{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Show (showLaws) where

import Hedgehog
import Hedgehog.Classes.Common

-- | Tests the following 'Show' laws:
--
-- [__ __]: @@ ≡ @@
-- [__ __]: @@ ≡ @@
-- [__ __]: @@ ≡ @@
showLaws :: (Show a) => Gen a -> Laws
showLaws gen = Laws "Show"
  [ ("Show", showShowsPrecZero gen)
  , ("Equivariance: showsPrec", equivarianceShowsPrec gen)
  , ("Equivariance: showList", equivarianceShowList gen)
  ]

showShowsPrecZero :: forall a. (Show a) => Gen a -> Property
showShowsPrecZero gen = property $ do
  a <- forAll gen
  let lhs = show a
  let rhs = showsPrec 0 a ""
  let ctx = contextualise $ LawContext
        { lawContextLawName = "ShowsPrec Zero", lawContextTcName = "Show"
        , lawContextLawBody = "show a" `congruency` "showsPrec 0 a \"\""
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a;
            in lawWhere
              [ "show a" `congruency` "showsPrec 0 a \"\", where"
              , "a = " ++ showA
              ]
        }  
  heqCtx lhs rhs ctx

equivarianceShowsPrec :: forall a. (Show a) => Gen a -> Property
equivarianceShowsPrec gen = property $ do
  p <- forAll genShowReadPrecedence
  a <- forAll gen
  r <- forAll genSmallString
  s <- forAll genSmallString
  let lhs = showsPrec p a r ++ s
  let rhs = showsPrec p a (r ++ s)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "ShowsPrec Equivariance", lawContextTcName = "Show"
        , lawContextLawBody = "showsPrec p a r ++ s" `congruency` "showsPrec p a (r ++ s)"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showP = show p; showA = show a; showR = show r; showS = show s;
            in lawWhere
              [ "showsPrec p a r ++ s" `congruency` "showsPrec p a (r ++ s), where"
              , "p = " ++ showP
              , "a = " ++ showA
              , "r = " ++ showR
              , "s = " ++ showS
              ]
        }  
  heqCtx lhs rhs ctx

equivarianceShowList :: forall a. (Show a) => Gen a -> Property
equivarianceShowList gen = property $ do
  as <- forAll $ genSmallList gen
  r <- forAll genSmallString
  s <- forAll genSmallString
  let lhs = showList as r ++ s
  let rhs = showList as (r ++ s)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "ShowList Equivariance", lawContextTcName = "Show"
        , lawContextLawBody = "showList as r ++ s" `congruency` "showList as (r ++ s)"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showAS = show as; showR = show r; showS = show s;
            in lawWhere
              [ "showList as r ++ s" `congruency` "showList as (r ++ s), where"
              , "as = " ++ showAS
              , "r = " ++ showR
              , "s = " ++ showS
              ]
        }  
  heqCtx lhs rhs ctx
