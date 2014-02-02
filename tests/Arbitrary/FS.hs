module Arbitrary.FS where
import Prelude hiding (FilePath, writeFile)
import Test.QuickCheck
import System.IO.Util
import Filesystem.Path.CurrentOS
import Arbitrary.File

import Control.Applicative

import Data.ModulePath

import qualified Data.Map as M
import qualified System.IO as IO
import qualified Data.List as L
import qualified Data.Set as S

import Control.Monad.State
import Data.Monoid (mempty)

newtype FS = FS { mapping :: M.Map FilePath Content }

instance Show FS where
  show = L.intercalate "\n" . map (uncurry fromPairing) . M.toList . mapping
    where
      fromPairing pth e =
        encodeString pth ++ ", " ++
          either (\t -> "TestModule: " ++ show t) (const "Nonsense") e

-- Produce a file-system containing 3-5 entries of tests or non-tests.
instance Arbitrary FS where
  -- FIXME adjust quantity
  arbitrary = FS <$> evalStateT (fromNumber mempty 1) (mempty, mempty) where
  --arbitrary = FS <$> (fromNumber M.empty =<< choose (1, 1)) where
    fromNumber ::
      M.Map FilePath Content
      -> Int
      -> StateT (S.Set FilePath, S.Set ModulePath) Gen (M.Map FilePath Content)
    fromNumber m 0 =
      return m
    fromNumber m n = do
      f <- fileGen (choose ('a', 'z'))
      fromNumber (M.insert (path f) (content f) m) (n - 1)

-- | Write an individual file to disk, note, for the test type, the path in
-- arg. is discarded so that pre-existing test writer can be used for
-- the sake of convenience.
writeFile :: FilePath -> FilePath -> Content -> IO ()
writeFile par rel_path c = do
  forceDirectories $ par </> parent rel_path
  IO.writeFile (encodeString complete_path) buffer
  where
    buffer =
      case c of
        (Left tm)            -> toStr tm
        (Right nonsense_buf) -> nonsense_buf
    complete_path =
      par </> rel_path

-- | Write the files to disk
writeFS :: FilePath -> FS -> IO ()
writeFS par = mapM_ (uncurry (writeFile par)) . M.toList . mapping
