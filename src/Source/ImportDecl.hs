module Source.ImportDecl where

import Language.Haskell.Exts (ModuleName, ImportSpec)

qualified :: Bool
qualified = True

noPackageName :: Maybe String
noPackageName = Nothing

noImportSpecs :: Maybe (Bool, [ImportSpec])
noImportSpecs = Nothing

noAs :: Maybe ModuleName
noAs = Nothing

noSrcPragma :: Bool
noSrcPragma = False
