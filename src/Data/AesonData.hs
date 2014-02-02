module Data.AesonData where

import Data.ModulePath
import Data.Suite.Ops
import Data.Aeson
import qualified Filesystem.Path.CurrentOS as FS
import Prelude hiding (FilePath, minimum, null)
import Control.Lens hiding ((.=))
import Data.Text (pack, Text)

import qualified Data.List as L
import qualified Data.Map as M
import Data.Integrated.TestModule (Map)

data AesonData = AesonData { suiteOps :: Ops, suitePath :: FS.FilePath }

instance ToJSON AesonData where
 toJSON p = object $ filepath : framework : members where
  filepath :: (Text, Value)
  filepath  = pack "suite-file-path" .= FS.encodeString (suitePath p)

  framework :: (Text, Value)
  framework = pack "framework" .= "test-framework-quickcheck2"

  members :: [(Text, Value)]
  members   =
    map (\(label, l) -> pack label .= fromMapping (view l (suiteOps p)))
      [
        ("added",     added)
      , ("removed",   removed)
      , ("modified",  modified)
      , ("unmodified",unmodified)
      ]

  fromMapping :: Map -> Value
  fromMapping = object . map (uncurry fromMember) . M.toList . M.map snd

  fromMember :: ModulePath -> [p] -> (Text, Value)
  fromMember mp properties = (pack . show $ mp, toJSON . L.length $ properties)
