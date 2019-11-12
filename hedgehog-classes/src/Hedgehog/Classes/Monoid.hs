{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Monoid (monoidLaws, commutativeMonoidLaws) where

import Hedgehog
import Hedgehog.Classes.Common

-- | Tests the following 'Monoid' laws:
--
-- [__Left Identity__]: @'mappend' 'mempty'@ ≡ @'id'@
-- [__Right Identity__]: @'flip' 'mappend' 'mempty'@ ≡ @'id'@
-- [__Associativity__]: @'mappend' a ('mappend' b c)@ ≡ @'mappend' ('mappend' a b) c@
-- [__Concatenation__]: @'mconcat'@ ≡ @'foldr' 'mappend' 'mempty'@
monoidLaws :: (Eq a, Monoid a, Show a) => Gen a -> Laws
monoidLaws gen = Laws "Monoid"
  [ ("Left Identity", monoidLeftIdentity gen)
  , ("Right Identity", monoidRightIdentity gen)
  , ("Associativity", monoidAssociative gen)
  , ("Concatenation", monoidConcatenation gen)
  ]

-- | Tests the following 'Monoid' laws:
--
-- [__Commutativity__]: @'mappend' a b@ ≡ @'mappend' b a@
commutativeMonoidLaws :: (Eq a, Monoid a, Show a) => Gen a -> Laws
commutativeMonoidLaws gen = Laws "Commutative Monoid"
  [ ("Commutativity", monoidCommutative gen)
  ]

monoidConcatenation :: forall a. (Eq a, Monoid a, Show a) => Gen a -> Property
monoidConcatenation gen = property $ do
  as <- forAll $ genSmallList gen
  let lhs = mconcat as
  let rhs = foldr mappend mempty as
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Concatenation", lawContextTcName = "Monoid"
        , lawContextLawBody = "mconcat" `congruency` "foldr mappend mempty"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showAS = show as; showMempty = show (mempty :: a);
            in lawWhere
              [ "mconcat as" `congruency` "foldr mappend mempty as, where"
              , "as = " ++ showAS
              , "mempty = " ++ showMempty
              ]
        }  
  heqCtx lhs rhs ctx

monoidAssociative :: forall a. (Eq a, Monoid a, Show a) => Gen a -> Property
monoidAssociative gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  let lhs = mappend a (mappend b c)
  let rhs = mappend (mappend a b) c
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Associativity", lawContextTcName = "Monoid"
        , lawContextLawBody = "mappend a (mappend b c)" `congruency` "mappend (mappend a b) c"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a; showB = show b; showC = show c;
            in lawWhere
              [ "mappend a (mappend b c)" `congruency` "mappend (mappend a b) c, where"
              , "a = " ++ showA
              , "b = " ++ showB
              , "c = " ++ showC
              ]
        }
  heqCtx lhs rhs ctx

monoidLeftIdentity :: forall a. (Eq a, Monoid a, Show a) => Gen a -> Property
monoidLeftIdentity gen = property $ do
  a <- forAll gen
  let lhs = mappend mempty a
  let rhs = a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Left Identity", lawContextTcName = "Monoid"
        , lawContextLawBody = "mappend mempty" `congruency` "id"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a; showMempty = show (mempty :: a);
            in lawWhere
              [ "mappend mempty a" `congruency` "a, where"
              , "a = " ++ showA
              , "mempty = " ++ showMempty
              ]
        }
  heqCtx lhs rhs ctx

monoidRightIdentity :: forall a. (Eq a, Monoid a, Show a) => Gen a -> Property
monoidRightIdentity gen = property $ do
  a <- forAll gen
  let lhs = mappend a mempty
  let rhs = a 
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Right Identity", lawContextTcName = "Monoid"
        , lawContextLawBody = "flip mappend mempty" `congruency` "id"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a; showMempty = show (mempty :: a);
            in lawWhere
              [ "mappend a mempty" `congruency` "a, where"
              , "a = " ++ showA
              , "mempty = " ++ showMempty
              ]
        }
  heqCtx lhs rhs ctx 

monoidCommutative :: forall a. (Eq a, Monoid a, Show a) => Gen a -> Property
monoidCommutative gen = property $ do
  a <- forAll gen
  b <- forAll gen
  let lhs = mappend a b
  let rhs = mappend b a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Commutativity", lawContextTcName = "Monoid (Commutative)"
        , lawContextLawBody = "mappend" `congruency` "flip mappend"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a; showB = show b;
            in lawWhere
              [ "mappend a b" `congruency` "mappend b a, where"
              , "a = " ++ showA
              , "b = " ++ showB
              ]
        }
  heqCtx lhs rhs ctx
