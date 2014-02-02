{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.QcSuite where
import Data.Suite
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Monoid

import Arbitrary.Suite
import Prelude hiding (FilePath)
import Control.Monad

main :: IO ()
main = void $quickCheckAll

instance Arbitrary Suite where
  arbitrary = flip genSuite mempty =<< choose (0,4)

prop_buf_isomorphism :: Suite -> Property
prop_buf_isomorphism (Suite m) =
  property $
    case fromBuf "" (toBuf [] m)  of
      Left  err -> error err
      Right m'  -> m' == m
prop_buf_isomorphism Uncreated = property True
