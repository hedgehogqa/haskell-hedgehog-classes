{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Hedgehog.Classes.Foldable (foldableLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (ErrorCall(..), try, evaluate)
import Data.Monoid (Sum(..), Endo(..), Dual(..))
import qualified Data.Foldable as Foldable

-- | Tests the following 'Foldable' laws:
--
-- [__Fold__]: @'Foldable.fold' ≡ 'Foldable.foldMap' 'id'@
-- [__FoldMap__]: @'Foldable.foldMap' f ≡ 'Foldable.foldr' ('mappend' '.' f) 'mempty'@
-- [__Foldr__]: @'Foldable.foldr' f z t ≡ 'appEndo' ('Foldable.foldMap' ('Endo' '.' f) t) z@
-- [__Foldr'__]: @'Foldable.foldr'' f z0 t ≡ 'Foldable.foldl' f' 'id' t z0, where f' k x z = k '$!' f x z@
-- [__Foldl__]: @'Foldable.foldl' f z t ≡ 'appEndo' ('getDual' ('Foldable.foldMap' ('Dual' '.' 'Endo' '.' 'flip' f) t)) z@
-- [__Foldl'__]: @'Foldable.foldl'' f z0 xs ≡ 'Foldable.foldr' f' 'id' xs z0, where f' x k z = k '$!' f z x@
-- [__Foldl1__]: @'Foldable.foldl1' f t ≡ let (x:xs) = 'Foldable.toList' t in 'foldl' f x xs@
-- [__Foldr1__]: @'Foldable.foldr1' f t ≡ let (xs,x)@ = @unsnoc ('Foldable.toList' t) in 'foldr' f x xs@
-- [__ToList__]: @'Foldable.toList' ≡ 'Foldable.foldr' (:) []@
-- [__Null__]: @'Foldable.null' ≡ 'Foldable.foldr' ('const' ('const' 'False')) 'True'@
-- [__Length__]: @'Foldable.length' ≡ 'getSum' '.' 'Foldable.foldMap' ('const' ('Sum' 1))@
--
-- This additionally tests that the user's implementations of 'Foldable.foldr'' and 'Foldable.foldl'' are strict in their accumulators.
foldableLaws ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
foldableLaws gen = Laws "Foldable"
  [ ("fold", foldableFold gen)
  , ("foldMap", foldableFoldMap gen)
  , ("foldr", foldableFoldr gen)
  , ("foldr'", foldableFoldr' gen)
  , ("foldl", foldableFoldl gen)
  , ("foldl'", foldableFoldl' gen)
  , ("foldl1", foldableFoldl1 gen)
  , ("foldr1", foldableFoldr1 gen)
  , ("toList", foldableToList gen)
  , ("null", foldableNull gen)
  , ("length", foldableLength gen)
  ]

foldableFold ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFold fgen = property $ do
  a <- forAll $ fgen $ genVerySmallList genSmallInteger
  let lhs = Foldable.fold a
  let rhs = Foldable.foldMap id a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Fold"
        , lawContextLawBody = "fold" `congruency` "foldMap id"
        , lawContextTcName = "Foldable"
        , lawContextTcProp =
            let showA = show a
            in lawWhere
              [ "fold a" `congruency` "foldMap id a, where"
              , "a = " ++ showA
              ]
        , lawContextReduced = reduced lhs rhs 
        } 

  heqCtx lhs rhs ctx

foldableFoldMap ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldMap fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  e <- forAll genQuadraticEquation
  let f = (:[]) . runQuadraticEquation e
  let lhs = Foldable.foldMap f a
  let rhs = Foldable.foldr (mappend . f) mempty a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "FoldMap"
        , lawContextLawBody = "foldMap f" `congruency` "foldr (mappend . f) mempty"
        , lawContextTcName = "Foldable"
        , lawContextTcProp =
            let showA = show a
                showF = "(:[]) $ " ++ show e
            in lawWhere
              [ "foldMap f a" `congruency` "foldr (mappend . f) mempty  a, where"
              , "f = " ++ showF
              , "a = " ++ showA
              ]
        , lawContextReduced = reduced lhs rhs 
        } 
  heqCtx lhs rhs ctx

foldableFoldr ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldr fgen = property $ do
  e <- forAll genLinearEquationTwo
  z <- forAll genSmallInteger
  t <- forAll $ fgen genSmallInteger
  let f = runLinearEquationTwo e
  let lhs = Foldable.foldr f z t
  let rhs = appEndo (Foldable.foldMap (Endo . f) t) z
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Foldr"
        , lawContextLawBody = "foldr f z t" `congruency` "appEndo (foldMap (Endo . f) t) z"
        , lawContextTcName = "Foldable"
        , lawContextTcProp =
            let showT = show t
                showF = show e
                showZ = show z
            in lawWhere
              [ "foldr f z t" `congruency` "appEndo (foldMap (Endo . f) t) z"
              , "f = " ++ showF
              , "z = " ++ showZ
              , "t = " ++ showT
              ]
        , lawContextReduced = reduced lhs rhs 
        } 
  heqCtx lhs rhs ctx
 
foldableFoldl ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldl fgen = property $ do
  e <- forAll genLinearEquationTwo
  z <- forAll genSmallInteger
  t <- forAll $ fgen genSmallInteger
  let f = runLinearEquationTwo e
  let lhs = Foldable.foldl f z t
  let rhs = appEndo (getDual (Foldable.foldMap (Dual . Endo . flip f) t)) z
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Foldl"
        , lawContextLawBody = "foldl f z t" `congruency` "appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z"
        , lawContextTcName = "Foldable"
        , lawContextTcProp =
            let showT = show t
                showF = show e
                showZ = show z
            in lawWhere
              [ "foldl f z t" `congruency` "appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z"
              , "f = " ++ showF
              , "z = " ++ showZ
              , "t = " ++ showT
              ]
        , lawContextReduced = reduced lhs rhs 
        } 
  heqCtx lhs rhs ctx

ctxNotStrict :: String -> Context
ctxNotStrict str = Context $ "Your implementation of " ++ str ++ " is not strict."

foldableFoldr' ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldr' fgen = property $ do
  xs <- forAll $ fgen (genBottom genSmallInteger)
  let f :: Bottom Integer -> Integer -> Integer
      f a b = case a of
        BottomUndefined -> error "foldableFoldr': your foldr' is not strict!"
        BottomValue v -> if even v then v else b
  z0 <- forAll genSmallInteger
  (rhs, ctx1) <- liftIO $ do
    let f' k x z = k $! f x z
    e <- try (evaluate (Foldable.foldl f' id xs z0))
    case e of
      Left (_ :: ErrorCall) -> pure (Nothing, ctxNotStrict "foldr'")
      Right i -> pure (Just i, NoContext)
  (lhs, ctx2) <- liftIO $ do
    e <- try (evaluate (Foldable.foldr' f z0 xs))
    case e of
      Left (_ :: ErrorCall) -> pure (Nothing, ctxNotStrict "foldr'")
      Right i -> pure (Just i, NoContext)
  let ctx = case ctx1 of
        NoContext -> case ctx2 of
          NoContext -> contextualise $ LawContext
            { lawContextLawName = "Foldr'"
            , lawContextLawBody = "foldr' f z0 t" `congruency` "foldl f' id t z0, where f' k x z = k $! f x z"
            , lawContextTcName = "Foldable"
            , lawContextTcProp =
                let showT = show xs
                    showF = "\\a b -> case a of\n  BottomUndefined -> error \"foldableFoldr': not strict\"\n  BottomValue v -> if even v then v else b"
                    showZ = show z0
                in lawWhere
                  [ "foldr' f z0 t" `congruency` "foldl f' id t z0, where f' k x z = k $! f x z"
                  , "f = " ++ showF
                  , "z0 = " ++ showZ
                  , "t = " ++ showT
                  ]
            , lawContextReduced = reduced lhs rhs 
            } 
          c2 -> c2
        c1 -> c1
  heqCtx lhs rhs ctx

foldableFoldl' ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldl' fgen = property $ do
  xs <- forAll $ fgen (genBottom genSmallInteger)
  let f :: Integer -> Bottom Integer -> Integer
      f a b = case b of
        BottomUndefined -> error "foldableFoldl': your foldl' is not strict!"
        BottomValue v -> if even v then a else v
  let z0 = 0
  (rhs,ctx1) <- liftIO $ do
    let f' x k z = k $! f z x
    e <- try (evaluate (Foldable.foldr f' id xs z0))
    case e of
      Left (_ :: ErrorCall) -> pure (Nothing, ctxNotStrict "foldl'")
      Right i -> pure (Just i, NoContext)
  (lhs,ctx2) <- liftIO $ do
    e <- try (evaluate (Foldable.foldl' f z0 xs))
    case e of
      Left (_ :: ErrorCall) -> pure (Nothing, ctxNotStrict "foldl'")
      Right i -> pure (Just i, NoContext) 
  let ctx = case ctx1 of
        NoContext -> case ctx2 of
          NoContext -> contextualise $ LawContext
            { lawContextLawName = "Foldl'"
            , lawContextLawBody = "foldl' f z0 xs" `congruency` "foldr f' id xs z0, where f' x k z = k $! f z x"
            , lawContextTcName = "Foldable"
            , lawContextTcProp =
                let showT = show xs
                    showF = "\\a b -> case a of\n  BottomUndefined -> error \"foldableFoldr': not strict\"\n  BottomValue v -> if even v then v else b"
                    showZ = show z0
                in lawWhere
                  [ "foldl' f z0 xs" `congruency` "foldr f' id xs z0, where f' x k z = k $! f z x"
                  , "f = " ++ showF
                  , "z0 = " ++ showZ
                  , "t = " ++ showT
                  ]
            , lawContextReduced = reduced lhs rhs 
            } 
          c2 -> c2
        c1 -> c1
  heqCtx lhs rhs ctx
 
foldableFoldl1 ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldl1 fgen = property $ do
  e <- forAll genLinearEquationTwo
  t <- forAll $ fgen genSmallInteger
  case compatToList t of
    [] -> success
    (x:xs) ->
      let f = runLinearEquationTwo e
          lhs = Foldable.foldl1 f t
          rhs = Foldable.foldl f x xs
          ctx = contextualise $ LawContext
            { lawContextLawName = "Foldl1"
            , lawContextLawBody = "foldl1 f t" `congruency` "let (x:xs) = toList t in foldl f x xs"
            , lawContextTcName = "Foldable"
            , lawContextTcProp =
                let showF = show e
                    showT = show t
                    showX = show x
                    showXS = show xs
                in lawWhere
                  [ "foldl1 f t" `congruency` "let (x:xs) = toList t in foldl f x xs, where"
                  , "f = " ++ showF
                  , "t = " ++ showT
                  , "x = " ++ showX
                  , "xs = " ++ showXS
                  ] 
            , lawContextReduced = reduced lhs rhs
            }
      in heqCtx lhs rhs ctx

foldableFoldr1 ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldr1 fgen = property $ do
  e <- forAll genLinearEquationTwo
  t <- forAll $ fgen genSmallInteger
  case unsnoc (compatToList t) of
    Nothing -> success
    Just (xs, x) ->
      let f = runLinearEquationTwo e
          lhs = Foldable.foldr1 f t
          rhs = Foldable.foldr f x xs
          ctx = contextualise $ LawContext
            { lawContextLawName = "Foldr1"
            , lawContextLawBody = "foldr1 f t" `congruency` "let (xs, x) = unsnoc (toList t) in foldr f x xs"
            , lawContextTcName = "Foldable"
            , lawContextTcProp =
                let showF = show e
                    showT = show t
                    showX = show x
                    showXS = show xs
                in lawWhere
                  [ "foldr1 f t" `congruency` "let (xs, x) = unsnoc (toList t) in foldr f x xs, where"
                  , "f = " ++ showF
                  , "t = " ++ showT
                  , "x = " ++ showX
                  , "xs = " ++ showXS
                  ]
            , lawContextReduced = reduced lhs rhs
            }
      in heqCtx lhs rhs ctx

foldableToList ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableToList fgen = property $ do
  t <- forAll $ fgen genSmallInteger
  let lhs = Foldable.toList t
  let rhs = Foldable.foldr (:) [] t
  let ctx = contextualise $ LawContext
        { lawContextLawName = "ToList"
        , lawContextLawBody = "toList" `congruency` "foldr (:) []"
        , lawContextTcName = "Foldable"
        , lawContextTcProp =
            let showT = show t
            in lawWhere
              [ "toList t" `congruency` "foldr (:) [] t, where"
              , "t = " ++ showT
              ]
        , lawContextReduced = reduced lhs rhs
        } 
  heqCtx lhs rhs ctx

foldableNull ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableNull fgen = property $ do
  t <- forAll $ fgen genSmallInteger
  let lhs = Foldable.null t
  let rhs = Foldable.foldr (const (const False)) True t
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Null"
        , lawContextLawBody = "null" `congruency` "foldr (const (const False)) True"
        , lawContextTcName = "Foldable"
        , lawContextTcProp =
            let showT = show t
            in lawWhere
              [ "null t" `congruency` "foldr (const (const False)) True t, where"
              , "t = " ++ showT
              ]
        , lawContextReduced = reduced lhs rhs
        } 
  heqCtx lhs rhs ctx  

foldableLength ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableLength fgen = property $ do
  t <- forAll $ fgen genSmallInteger
  let lhs = Foldable.length t
  let rhs = getSum (Foldable.foldMap (const (Sum 1)) t)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Length"
        , lawContextLawBody = "length" `congruency` "getSum . foldMap (const (Sum 1))"
        , lawContextTcName = "Foldable"
        , lawContextTcProp =
            let showT = show t
            in lawWhere
              [ "length t" `congruency` "getSum . foldMap (const (Sum 1)) $ t, where"
              , "t = " ++ showT
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = \case
  [] -> Nothing
  [x] -> Just ([], x)
  (x:y:xs) -> fmap (\(bs,b) -> (x:bs,b)) (unsnoc (y : xs))

compatToList :: Foldable f => f a -> [a]
compatToList = Foldable.foldMap (\x -> [x])
