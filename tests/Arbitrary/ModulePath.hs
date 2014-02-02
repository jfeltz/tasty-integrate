{-# OPTIONS_GHC
  -fno-warn-unused-imports
  -fno-warn-missing-signatures
  -fno-warn-orphans
  -fno-warn-unused-imports
  -fno-warn-unused-binds
  -fno-warn-unused-do-bind
  #-}
module Arbitrary.ModulePath where
import Test.QuickCheck
import Data.Char
import Data.ModulePath

import Control.Applicative hiding (empty)
import Prelude hiding (FilePath)
import qualified Data.List as L

data Module = Module { str :: String } deriving Show

instance Arbitrary Module where
  arbitrary = toModule $ choose ('a','p')

toModule :: Gen Char -> Gen Module
toModule subpath = do
  modLen <- choose (1, 2)
  uncased <- vectorOf modLen subpath
  return . Module $ L.map toUpper (take 1 uncased) ++ L.drop 1 uncased

instance Arbitrary ModulePath where
  arbitrary = do
    pathLen <- choose (1, 3)
    ModulePath . map str <$> vectorOf pathLen arbitrary

toModulePath :: Gen Char -> Gen ModulePath
toModulePath subpath = do
  pathLen <- choose (1, 2)
  ModulePath . map str <$> vectorOf pathLen (toModule subpath)
