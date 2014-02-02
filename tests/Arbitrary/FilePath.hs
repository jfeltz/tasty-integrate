module Arbitrary.FilePath where
import Test.QuickCheck
import Control.Applicative
import qualified Filesystem.Path.CurrentOS as FS
import Prelude hiding (FilePath)
import qualified Data.Text as T

filepathGen :: Gen Char -> Gen FS.FilePath
filepathGen subpath = do
  pathLen <- choose (1, 3)
  ext     <- oneof . map return $ ["txt", "dat", "o"]
  flip FS.addExtension (T.pack ext)
    . foldl FS.append FS.empty
    . map (FS.decodeString . (:[]))
    <$> vectorOf pathLen subpath
