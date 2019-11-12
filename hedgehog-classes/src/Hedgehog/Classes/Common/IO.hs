module Hedgehog.Classes.Common.IO
  ( genIO
  , showIO
  ) where

import Hedgehog
import System.IO.Unsafe (unsafePerformIO)

genIO :: Gen a -> Gen (IO a)
genIO gen = fmap pure gen

showIO :: Show a => IO a -> String
showIO io = unsafePerformIO $ do
  x <- fmap show io
  let y = "IO " ++ x
  pure y
