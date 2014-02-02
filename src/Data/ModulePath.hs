module Data.ModulePath where
import qualified Data.List as L
import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.Text as T
import Data.List.Split
import Data.Char

data ModulePath = ModulePath { modules :: [String] } deriving (Eq, Ord)

instance Show ModulePath where
  show = L.intercalate "." . modules

-- TODO I'm unsure if this instance voliates some best-practice related
-- to Read
instance Read ModulePath where
  readsPrec _ s =
    let tokens = splitOn "." s in
      if any (not . legalModSubpath) tokens then
        []
      else
        [(ModulePath tokens, "")]

-- | Produce a relative file path given a module-path
relPath :: ModulePath -> FS.FilePath
relPath m = FS.addExtension unextended (T.pack "hs")
  where unextended = L.foldl (FS.</>) FS.empty $ map FS.decodeString (modules m)

-- Note by precondition, this assumes that the full path forms a module,
-- and cases which don't E.g: a/B/C.hs will produce a.B.C.
fromFilePath :: FS.FilePath -> ModulePath
fromFilePath p =
  fromParent (FS.parent p) $ ModulePath [FS.encodeString $ FS.basename p]
  where
    fromParent :: FS.FilePath -> ModulePath -> ModulePath
    fromParent next built =
      if FS.parent next == next then
        built
      else
        fromParent (FS.parent next) $
          ModulePath (FS.encodeString (FS.dirname next) : modules built)

legalModSubpath :: String -> Bool
legalModSubpath []   =
  False
legalModSubpath name =
  valid_first (head name) && all isAlphaNum name
    where valid_first c = isUpper c && isAlpha c

-- This returns a tuple consisting of the unrecognized portion
-- of the module path, of which may or may not contain a file
parseModFilePath :: FS.FilePath -> (FS.FilePath, FS.FilePath)
parseModFilePath p =
  if FS.filename p == FS.empty then
    if FS.dirname p == FS.empty then
      (FS.empty, FS.empty)
    else
      fromDirs p FS.empty
  else
    let file = FS.filename p
        t = fromDirs (FS.parent p) FS.empty
    in
      if legalModSubpath $ FS.encodeString (FS.basename file) then
        (fst t, FS.append (snd t) file) else (p, FS.empty)
    where
      fromDirs :: FS.FilePath -> FS.FilePath -> (FS.FilePath, FS.FilePath)
      fromDirs next built =
        if FS.parent next == next then
          (FS.empty, built)
        else let current = FS.dirname next in
          if legalModSubpath $ FS.encodeString current then
            fromDirs
              (FS.parent next)
              (FS.append current $ if FS.null built then FS.empty else built)
          else
            (FS.append next FS.empty, built)

pathToMod :: FS.FilePath -> String
pathToMod = show . fromFilePath . snd . parseModFilePath

toPath :: FS.FilePath -> ModulePath -> FS.FilePath
toPath par = FS.append par . relPath
