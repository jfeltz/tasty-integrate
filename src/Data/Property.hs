module Data.Property where

import Language.Haskell.Exts.Syntax
import Text.Regex.Posix (makeRegex, match, Regex)
import Data.Char

data Property = Property { line :: Int, func :: String }

instance Read Property where
  readsPrec _ s =
    let (first, second) = span (/= ':') s in
      [(Property (read first :: Int) (dropWhile isSpace (drop 1 second)), [])]

instance Show Property where
  show p = show (line p) ++ ": " ++ func p

instance Ord Property where
  compare l r = compare (func l) (func r)

instance Eq Property where
  (==) l r = show l == show r

isProp :: String -> Bool
isProp = match (makeRegex "^prop_" :: Regex)

fromDecl :: Decl -> Maybe Property
fromDecl decl = do
  (loc, name) <- pairing
  if isProp name
    then Just $ Property loc name
    else Nothing
  where
    pairing :: Maybe (Int, String)
    pairing =
      case decl of
        (PatBind srcloc (PVar (Ident n)) _ _ _) ->
          Just (srcLine srcloc, n)
        (FunBind ms)                            ->
          Just (fromMatch $ head ms)
        _                                       ->
          Nothing
        where
          fromMatch (Match srcloc expr _ _ _ _)  = (srcLine srcloc, name) where
            name = case expr of
                     (Symbol n) -> n
                     (Ident  n) -> n
