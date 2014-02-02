module Arbitrary.TestFS where
import Test.QuickCheck
--import Test.Framework.Providers.QuickCheck2
--import Env
import Filesystem.Path.CurrentOS
import Prelude hiding (writeFile, FilePath)
--import Data.Path
import Arbitrary.FS (writeFile)
import qualified Data.Set as S
import Control.Applicative
import Data.ModulePath

import Arbitrary.File

write :: FilePath -> TestFS -> IO ()
write sandbox (NonDir f)   = writeFile sandbox (path f) (content f)
write sandbox (Root files) = writeFiles sandbox files
write _       (NotFound _) = return ()

--This stores the state of a generated file-system
--prior to integration

data TestFS = NonDir File | Root Files | NotFound FilePath

instance Show TestFS where
  show (NonDir f)   = "A file: " ++ '\n': show f
  show (Root files) = "Files: " ++ '\n' : show files
  show (NotFound p) = "Non-existent: " ++ '\n' : show p

-- Generated sync is either non-existent, or a random directory, or
-- a random set of files
instance Arbitrary TestFS where
  arbitrary =
    oneof [
      NonDir <$> fileGen (choose ('a','c')) S.empty
      , Root <$> (arbitrary :: Gen Files)
      , NotFound . relPath <$> testModulePath (choose ('a','c')) S.empty
      ]
