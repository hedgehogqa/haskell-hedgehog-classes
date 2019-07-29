module Spec.Category (testCategory, testCommutativeCategory) where

import Control.Category
import Hedgehog
import Hedgehog.Classes
import Prelude hiding ((.), id)

testCategory :: [(String, [Laws])]
testCategory =
  [ ("ProxyC", [categoryLaws genProxyC])
  ]

testCommutativeCategory :: [(String, [Laws])]
testCommutativeCategory =
  [ ("ProxyC", [commutativeCategoryLaws genProxyC])
  ]

data ProxyC a b = ProxyC
  deriving (Eq, Show)

instance Category ProxyC where
  id = ProxyC
  _ . _ = ProxyC

genProxyC :: Gen a -> Gen b -> Gen (ProxyC a b)
genProxyC _ _ = pure ProxyC
