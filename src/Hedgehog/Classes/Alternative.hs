{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}

module Hedgehog.Classes.Alternative (alternativeLaws) where

import Control.Applicative (Alternative(..))

import Hedgehog
import Hedgehog.Classes.Common

alternativeLaws ::
  ( Alternative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
alternativeLaws gen = Laws "Alternative"
  [ ("Left Identity", alternativeLeftIdentity gen)
  , ("Right Identity", alternativeRightIdentity gen)
  , ("Associativity", alternativeAssociativity gen)
  ]

type AlternativeProp f =
  ( Alternative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property

alternativeLeftIdentity :: forall f. AlternativeProp f
alternativeLeftIdentity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  let lhs = empty <|> a
  let rhs = a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Left Identity", lawContextLawBody = "empty <|> a" `congruency` "a"
        , lawContextTcName = "Alternative", lawContextTcProp =
            let showA = show a;
            in lawWhere
              [ "empty <|> a" `congruency` "a, where"
              , "a = " ++ showA
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx1 lhs rhs ctx

alternativeRightIdentity :: forall f. AlternativeProp f
alternativeRightIdentity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  let lhs = a <|> empty
  let rhs = a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Right Identity", lawContextLawBody = "a <|> empty" `congruency` "a"
        , lawContextTcName = "Alternative", lawContextTcProp =
            let showA = show a;
            in lawWhere
              [ "a <|> empty" `congruency` "a, where"
              , "a = " ++ showA
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx1 lhs rhs ctx  

alternativeAssociativity :: forall f. AlternativeProp f
alternativeAssociativity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  b <- forAll $ fgen genSmallInteger
  c <- forAll $ fgen genSmallInteger
  let lhs = (a <|> (b <|> c))
  let rhs = ((a <|> b) <|> c)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Associativity", lawContextLawBody = "a <|> (b <|> c)" `congruency` "(a <|> b) <|> c"
        , lawContextTcName = "Alternative", lawContextTcProp =
            let showA = show a; showB = show b; showC = show c;
            in lawWhere
                 [ "a <|> (b <|> c)" `congruency` "(a <|> b) <|> c), where"
                 , "a = " ++ showA
                 , "b = " ++ showB
                 , "c = " ++ showC
                 ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx1 lhs rhs ctx
