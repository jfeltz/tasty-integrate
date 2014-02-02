{-# OPTIONS_GHC -fno-warn-unused-matches  -fno-warn-unused-binds #-}
module Data.Path where
import System.Directory
import qualified Filesystem.Path.CurrentOS as FS
import Prelude hiding (FilePath)

data NodeType = Directory FS.FilePath | File FS.FilePath deriving (Show)
data Path = Existing NodeType | NotExisting String deriving (Show)

encodeString :: Path -> String
encodeString (Existing (Directory p)) = FS.encodeString p
encodeString (Existing (File p))      = FS.encodeString p
encodeString (NotExisting str)        = str

toPath :: String -> IO Path
toPath src = do
  directoryExists <- doesDirectoryExist src
  fileExists      <- doesFileExist src
  return $
    if not directoryExists && not fileExists then
      NotExisting src
    else
      Existing . (if fileExists then File else Directory) $
        FS.decodeString src
