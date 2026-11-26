{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hedgehog.Classes.Binary (binaryLaws) where

import Hedgehog
import Hedgehog.Classes.Common
import Data.Binary (Binary)
import qualified Data.Binary as Binary

-- | Tests the following 'Binary' laws:
--
-- [__Encoding Partial Isomorphism__]: @'Binary.decode' '.' 'Binary.encode'@ ≡ @'id'@, using up all of the input
--
-- An instance whose 'Binary.put' writes bytes that its 'Binary.get' doesn't
-- read gives back the value it encoded, but breaks any encoding that puts it
-- beside another value, such as a list or a tuple: so the law decodes with
-- 'Binary.decodeOrFail', and fails when input is left over.
binaryLaws :: (Binary a, Eq a, Show a) => Gen a -> Laws
binaryLaws gen = Laws "Binary"
  [ ("Partial Isomorphism", binaryPartialIsomorphism gen)
  ]

binaryPartialIsomorphism :: forall a. (Binary a, Show a, Eq a) => Gen a -> Property
binaryPartialIsomorphism gen = property $ do
  x <- forAll gen
  let encoded = Binary.encode x
  let lhs = (\(rest, _, y) -> (rest, y)) <$> Binary.decodeOrFail @a encoded
  let rhs = Right (mempty, x)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Partial Isomorphism", lawContextTcName = "Binary"
        , lawContextLawBody = "decode . encode" `congruency` "id, using up all of the input"
        , lawContextTcProp =
            let showX = show x
                showEncoded = show encoded
            in lawWhere
              [ "decodeOrFail (encode x)" `congruency` "Right (empty, _, x), where"
              , "x = " ++ showX
              , "encode x = " ++ showEncoded
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx
