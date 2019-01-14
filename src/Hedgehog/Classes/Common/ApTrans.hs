module Hedgehog.Classes.Common.ApTrans
  ( apTrans
  , toSpecialApplicative
  ) where

import Data.Tuple (swap)
import Data.Functor.Compose
import qualified Data.Set as S
import qualified Control.Monad.Trans.Writer.Lazy as WL

import Hedgehog.Classes.Common.Func

-- Reverse the list and accumulate the writers. We
-- cannot use Sum or Product or else it won't actually
-- be a valid applicative transformation.
apTrans ::
     Compose Triple (WL.Writer (S.Set Integer)) a
  -> Compose (WL.Writer (S.Set Integer)) Triple a
apTrans (Compose xs) = Compose (sequenceA (reverseTriple xs))

toSpecialApplicative ::
     Compose Triple ((,) (S.Set Integer)) Integer
  -> Compose Triple (WL.Writer (S.Set Integer)) Integer
toSpecialApplicative (Compose (Triple a b c)) =
  Compose (Triple (WL.writer (swap a)) (WL.writer (swap b)) (WL.writer (swap c)))
