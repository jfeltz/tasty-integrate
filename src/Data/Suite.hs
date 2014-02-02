{-# LANGUAGE TupleSections #-}
module Data.Suite where
import Data.ModulePath
import Data.Integrated.TestModule (ModuleMapped, Map)
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc
import Data.Property
import Source.ImportDecl
import Prelude hiding (FilePath, exp, otherwise, error)

import qualified Data.Map as M
import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.List as L
import Data.Monoid (mempty)
import Control.Applicative

data Suite =
  Uncreated | Suite { tests :: ModuleMapped (FS.FilePath, [Property]) }
    deriving (Eq, Show)

fromGroupExp :: Exp -> Either String (ModulePath, (FS.FilePath, [Property]))
fromGroupExp (App
                (App (Var (UnQual ( Ident fname ))) (Lit (String test_file)))
                (List prop_exps)
             ) =
  let
    test_fp :: FS.FilePath
    test_fp = FS.decodeString test_file
    groupModule :: ModulePath
    groupModule = fromFilePath . snd . parseModFilePath $ test_fp
  in
  if fname /= "testGroup" then
    Left "missing testGroup"
  else do
    properties <- fromProps groupModule prop_exps
    return (groupModule, (test_fp, properties))
  where
    -- Note, each property method is qualified,
    -- and this how the source test module is inferred,
    -- not from the test group label
    fromProps :: ModulePath -> [Exp] -> Either String [Property]
    fromProps group_mod (e:exps) =
      case e of
        (App
          (App(Var(UnQual(Ident prop_f))) (Lit ( String p_str )))
          (Var (Qual (ModuleName modname) _))
          ) ->
          if prop_f == "testProperty" then do
            properties <- fromProps group_mod exps
            if read modname /= group_mod
              then
                Left "property qualification doesn't match group module"
              else
                Right $ read p_str : properties
          else
            Left "non-property (expected testProperty) encountered in test grouping"
        _ -> Left "non testProperty found in group expression"
    fromProps _ []               = Right []
fromGroupExp _ = Left "unable to match group expression"

fromModuleGroup :: Exp -> Either String (ModuleMapped (FS.FilePath, [Property]))
fromModuleGroup (Paren (App
                  (App (Var (UnQual ( Ident fname ))) (Lit (String _)))
                  module_exps
                )) =
  if fname == "testGroup" then
    fromModuleGroups module_exps
  else
    Left "missing module-groups"
  where
    fromModuleGroups (List [])         = Right mempty
    fromModuleGroups (List (g:groups)) = do
      (mp, properties) <- fromGroupExp g
      M.insert mp properties <$> fromModuleGroups (List groups)
    fromModuleGroups _                 =
      Left "wrong module-group type (should be list)"
fromModuleGroup _                 =
  Left "missing test module-groups"

fromBuf :: String -> String -> Either String Map
fromBuf path_str buf =
  case parseModule buf of
    ParseFailed location err ->
      Left $ path_str ++
              ':' : show (srcLine location) ++
              ':' : ("err" ++ show (srcColumn location) ++ err)
    ParseOk (Module _ _ _ _  _  _ []) ->
      Left $ path_str ++ ": contains no declarations"
    ParseOk (Module _ _ _ _  _  _ decls) ->
      fromMain $ decls L.!! 1
  where
    fromMainExp :: Exp -> Either String (ModuleMapped (FS.FilePath, [Property]))
    fromMainExp (App ( Var ( UnQual ( Ident fname ) ) ) exp) =
      if fname == "defaultMain" then
        fromModuleGroup exp
      else
        Left "unable to locate defaultMain in suite"
    fromMainExp _ =
      Left "unable to locate defaultMain in suite"
    fromMain :: Decl -> Either String (ModuleMapped (FS.FilePath, [Property]))
    fromMain (PatBind _ (PVar _)  _ (UnGuardedRhs exp) _) =
      fromMainExp exp
    fromMain _ =
      Left "unable to locate main binding in suite"

toModule :: [ModulePath] -> ModuleMapped (FS.FilePath, [Property]) -> Module
toModule import_list m =
  Module
    noLoc
    (ModuleName "Main")
    []
    Nothing
    Nothing
    (map
      (uncurry testImport)
      (M.toList $ M.map snd m) ++ map suiteImport import_list
      )
    [
      TypeSig noLoc [Ident "main"]
        (TyApp (TyCon (UnQual (Ident "IO"))) (TyCon (Special UnitCon)))
      ,
      nameBind noLoc (name "main") $
        appFun (function "defaultMain")
          [
            appFun (function "testGroup") [
              strE "modules",
              listE . map (uncurry toGroup) . M.toList $ m
              ]
          ]
      ]
  where
    suiteImport :: ModulePath -> ImportDecl
    suiteImport mp =
      ImportDecl
        noLoc
        (ModuleName . show $ mp)
        (not qualified)
        noSrcPragma
        noPackageName
        noAs
        noImportSpecs

    testImport :: ModulePath -> [Property] -> ImportDecl
    testImport mp properties =
      ImportDecl
        noLoc
        (ModuleName . show $ mp)
        qualified
        noSrcPragma
        noPackageName
        noAs
        (Just (False, map toImportSpec properties))

    toImportSpec :: Property -> ImportSpec
    toImportSpec p = IVar ( name $ func p )

    toGroup ::  ModulePath -> (FS.FilePath, [Property]) -> Exp
    toGroup mp (non_module_path, prop_list) =
      appFun
        (function "testGroup")
        [(strE . FS.encodeString) non_module_path, props]
      where
        props = listE $ map (propExp (ModuleName . show $ mp)) prop_list
        propExp :: ModuleName -> Property -> Exp
        propExp mod_name p =
          appFun
            (function "testProperty")
            [strE (show p), qvar mod_name (name $ func p)]

tastyImports :: [ModulePath]
tastyImports = map read [ "Test.Tasty" , "Test.Tasty.QuickCheck" ]

-- This composes the full path of a module given its parent path and a
-- the module path

toBuf :: [ModulePath] -> ModuleMapped (FS.FilePath, [Property]) -> String
toBuf import_list m =
  L.intercalate "\n" [
    "-- WARNING: This file is generated by tasty-integrate\n" ++
    "-- Changes made will (probably) not be saved, see documentation."
    , "{-# OPTIONS_GHC -fno-warn-missing-signatures #-}"
    , prettyPrint (toModule import_list m)
    ]

toSpecs :: [Property] -> Maybe (Bool, [ImportSpec])
toSpecs props = Just (False, map (IVar . name . func) props)
