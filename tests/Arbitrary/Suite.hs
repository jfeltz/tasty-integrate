{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arbitrary.Suite where
import Data.Suite
--import Data.Test hiding (fromBuf)
import Arbitrary.TestModule
import Test.QuickCheck
import Prelude hiding (FilePath)

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Data.Integrated.TestModule

genSuite :: Int -> Map -> Gen Suite
genSuite 0         mask =
  return . Suite $ mask
genSuite num_tests mask = do
  (p, tm) <- toGenerated (choose ('a', 'z')) (S.fromList . M.keys $ mask)
  -- (p, t) <- testGen (choose ('a', 'z')) (S.fromList . M.keys $ mask)
  genSuite (num_tests - 1) $
    M.insert (view modpath tm)  (p, view properties tm) mask
