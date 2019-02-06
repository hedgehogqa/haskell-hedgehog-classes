{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Json (jsonLaws) where

import Hedgehog
import Hedgehog.Classes.Common
import Data.Aeson (FromJSON, ToJSON(toJSON))
import qualified Data.Aeson as Aeson

jsonLaws :: (FromJSON a, ToJSON a, Eq a, Show a) => Gen a -> Laws
jsonLaws gen = Laws "ToJSON/FromJSON"
  [ ("Partial Isomorphism", jsonEncodingPartialIsomorphism gen)
  , ("Encoding equals value", jsonEncodingEqualsValue gen)
  ]

jsonEncodingPartialIsomorphism :: forall a. (ToJSON a, FromJSON a, Show a, Eq a) => Gen a -> Property
jsonEncodingPartialIsomorphism gen = property $ do
  x <- forAll gen
  let encoded = Aeson.encode x
  let lhs = Aeson.decode encoded
  let rhs = Just x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Partial Isomorphism", lawContextTcName = "ToJSON/FromJSON"
        , lawContextLawBody = "decode . encode" `congruency` "Just"
        , lawContextTcProp =
            let showX = show x
                showEncoded = show encoded
            in lawWhere
              [ "decode . encode $ x" `congruency` "Just x, where"
              , "x = " ++ showX
              , "encode x = " ++ showEncoded
              ]
        , lawContextReduced = reduced lhs rhs
        }  
  heqCtx lhs rhs ctx

jsonEncodingEqualsValue :: forall a. (ToJSON a, Show a) => Gen a -> Property
jsonEncodingEqualsValue gen = property $ do
  x <- forAll gen
  let encoded = Aeson.encode x
  let decoded = Aeson.decode encoded :: Maybe Aeson.Value
  let lhs = decoded
  let rhs = Just (toJSON x)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Encoding equals value", lawContextTcName = "ToJSON"
        , lawContextLawBody = "decode . encode" `congruency` "Just . toJSON"
        , lawContextTcProp =
            let showX = show x
                showEncoded = show encoded
                showDecoded = show decoded
            in lawWhere
              [ "decode . encode $ x" `congruency` "Just . toJSON, where"
              , "x = " ++ showX
              , "encoded = " ++ showEncoded
              , "decoded = " ++ showDecoded
              ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx 

