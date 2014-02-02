{-# LANGUAGE TemplateHaskell #-}
module Data.Integrated.TestModule where
import Data.List
import Data.List.Split
import System.IO.Util
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc (noLoc)
import Data.ModulePath
import Data.Property
import Data.Maybe
import qualified Filesystem.Path.CurrentOS as FS
import Prelude hiding (mod, FilePath, error, last)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Lens
import Source.ImportDecl

data TestModule =
  TestModule { _modpath :: ModulePath, _properties :: [Property] }
$(makeLenses ''TestModule)

instance Show TestModule where
  show t =
    show (_modpath t) ++ ":\n " ++ intercalate "\n" (map show (_properties t))

instance Eq TestModule where
  (==) l r = _modpath l == _modpath r && _properties l == _properties r

instance Ord TestModule where
  compare l r =
    let testModpathcomp = compare (_modpath l) (_modpath r) in
      if testModpathcomp == EQ then
        compare (_properties l) (_properties r)
      else
        testModpathcomp

fromtestModpath :: String -> FS.FilePath
fromtestModpath testModpath' =
  fromElems $ splitOn "." testModpath'
  where
    fromElems []           = FS.empty
    fromElems (f:[])       =
      FS.decodeString (f ++ ".hs")
    fromElems (subpath:rest) =
      FS.append (FS.decodeString subpath) (fromElems rest)

hasFunc :: String -> Either String Bool
hasFunc buf =
  case parseModuleWithMode (defaultParseMode { fixities = Nothing }) buf of
    ParseFailed loc' err -> Left (show loc' ++ ' ':err)
    ParseOk m            -> (Right . containsFunc) (moduleDecls m)
  where
    moduleDecls (Module _ _ _ _ _ _ d) = d
    containsFunc []                    = False
    containsFunc (FunBind {} : _)      = True
    containsFunc (PatBind {} : _)      = True
    containsFunc (_:rest)              = containsFunc rest

moduleMapped :: S.Set TestModule -> M.Map ModulePath TestModule
moduleMapped = M.fromList . S.toList . S.map (\t -> (_modpath t, t))

toModule :: [ImportDecl] -> ModulePath -> Module
toModule imports mp =
  Module
    noLoc
    (ModuleName . show $ mp)
    [LanguagePragma noLoc [Ident "TemplateHaskell"]]
    Nothing
    Nothing
    imports
    [TypeSig
       noLoc
       [Ident "main"]
       (TyApp (TyCon (UnQual (Ident "IO")))
       (TyCon (Special UnitCon)))
     , nameBind noLoc (name "main") $
        SpliceExp (ParenSplice (Var (UnQual (Ident "defaultMainGenerator"))))
    ]

writeModule :: FS.FilePath -> [ImportDecl] -> ModulePath -> IO ()
writeModule parent_dir imports mp =
  let filepath = toPath parent_dir mp in do
    forceDirectories $ FS.parent filepath
    writeFile (FS.encodeString filepath) (prettyPrint $ toModule imports mp)

toImports :: [String] -> [ImportDecl]
toImports [] = []
toImports (i:imps) =
  fromStr i : toImports imps where
    fromStr s =
      ImportDecl
        noLoc
        (ModuleName s)
        (not qualified)
        False
        noPackageName
        noAs
        noImportSpecs

-- Note, haskell-src-exts horribly throws exceptions in a "pure" parse
-- so we treat this as a file specific error
fromBuf :: String -> Either String TestModule
fromBuf buf =
  let
    relaxed_mode =
      defaultParseMode {
        extensions =
          EnableExtension TemplateHaskell : knownExtensions
        -- TODO: fixities is haskell-src-exts 1.14.0 specific workaround
        -- otherwise you get bugged false-exception, ie: "ambiguous
        -- fixities"
        , fixities = Just []
      }
  in
  case parseModuleWithMode relaxed_mode buf of
    ParseFailed srcloc error_str ->
      Left $ fmtError srcloc error_str
    ParseOk (Module _ (ModuleName mod_name) _ _ _ _ d) ->
      Right $ TestModule (read mod_name) (mapMaybe fromDecl d)

fmtError :: SrcLoc -> String -> String
fmtError srcloc err = li ++ ':':col ++ ' ':err
  where
    li = show $ srcLine srcloc
    col = show $ srcColumn srcloc

type ModuleMapped a = M.Map ModulePath a
type Map = ModuleMapped (FS.FilePath, [Property])
