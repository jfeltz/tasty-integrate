{-# LANGUAGE TupleSections #-}
module Arbitrary.TestModule where
import Data.Integrated.TestModule
import Test.QuickCheck
import Data.ModulePath
import Control.Applicative
import Arbitrary.Properties
import Test.Util
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)

import qualified Arbitrary.ModulePath as MP
import qualified Data.Set as S

testModulePath :: Gen Char -> S.Set ModulePath -> Gen ModulePath
testModulePath subpath avoided =
  suchThat
    (fromModPath <$> MP.toModulePath subpath)
    (not . flip S.member avoided)
  where
    fromModPath :: ModulePath -> ModulePath
    fromModPath (ModulePath pth) =
      ModulePath $ take (length pth - 1) pth ++ [testFormat $ last pth]

toTestModule :: ModulePath -> Gen TestModule
toTestModule mp = do
  props <- arbitrary :: Gen Properties
  return $ TestModule mp (list props)

-- Generate a random test file, care must be taken to avoid generating
-- the same path twice
toGenerated :: Gen Char -> S.Set ModulePath -> Gen (FilePath, TestModule)
toGenerated subpath avoided = do
  mp <- testModulePath subpath avoided
  (relPath mp,) <$> toTestModule mp
