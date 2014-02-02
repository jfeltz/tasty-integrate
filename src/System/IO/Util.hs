module System.IO.Util where

import Filesystem.Path ((</>), FilePath, dirname, parent)
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import Prelude hiding (FilePath, error, last)
import System.Directory
import System.IO (openFile, IOMode( ReadMode ), Handle)
import System.IO.Error
import Control.Applicative
import Control.Monad (forM)

forceDirectories :: FilePath -> IO ()
forceDirectories path =
  mapM_  (createDir path) (subPaths path) where
    subPaths p = prev p ++ [p] where
      prev child = if parent child == child then
                     []
                   else subPaths $ parent child
    createDir p subpath =
      do
        let --Remove the trailing directory seperator so that
            --doesFileExist works as intended
            unsep_subpath = parent subpath </> dirname subpath
            errstr =
              "file: " ++ show unsep_subpath ++ " exists in directory path: " ++
                encodeString p

        exists <- doesFileExist (encodeString unsep_subpath)
        if exists
          then ioError $
            mkIOError illegalOperationErrorType errstr Nothing Nothing
          else createDirectoryIfMissing False $ encodeString subpath

maybeHandle :: FilePath -> IO (Maybe Handle)
maybeHandle p =
  do
    let p_str = encodeString p
    exists <- doesFileExist p_str
    if exists then Just <$> openFile p_str ReadMode
       else return Nothing

--Derived from:
-- http://book.realworldhaskell.org/ getDirectoryContents
toDirectoryLeafs :: FilePath -> IO [FilePath]
toDirectoryLeafs dir = do
  names <-
    map decodeString . filter (`notElem` [".", ".."]) <$>
       getDirectoryContents (encodeString dir)
  paths <-
    -- Note, getDirectoryContents provides a relative paths only,
    -- and by pre-condition the dir is the parent, so
    -- to hold the post-condition full path, the relative is prefixed
    forM names $ \name -> do
      let path = dir </> name
      isDirectory <- doesDirectoryExist $ encodeString path
      if isDirectory
        then toDirectoryLeafs path
        else return [path]
  return (concat paths)
