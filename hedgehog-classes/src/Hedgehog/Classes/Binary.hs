{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hedgehog.Classes.Binary (binaryLaws) where

import Hedgehog
import Hedgehog.Classes.Common
import Data.Binary (Binary)
import qualified Data.Binary as Binary

-- | Tests the following 'Binary' laws:
--
-- [__Encoding Partial Isomorphism__]: @'Binary.decode' '.' 'Binary.encode'@ ≡ @'id'@
binaryLaws :: (Binary a, Eq a, Show a) => Gen a -> Laws
binaryLaws gen = Laws "Binary"
  [ ("Partial Isomorphism", binaryPartialIsomorphism gen)
  ]

binaryPartialIsomorphism :: forall a. (Binary a, Show a, Eq a) => Gen a -> Property
binaryPartialIsomorphism gen = property $ do
  x <- forAll gen
  let encoded = Binary.encode x
  let lhs = Binary.decode @a encoded
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Partial Isomorphism", lawContextTcName = "Binary"
        , lawContextLawBody = "decode . encode" `congruency` "id"
        , lawContextTcProp =
            let showX = show x
                showEncoded = show encoded
            in lawWhere
              [ "decode . encode $ x" `congruency` "x, where"
              , "x = " ++ showX
              , "encode x = " ++ showEncoded
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx
