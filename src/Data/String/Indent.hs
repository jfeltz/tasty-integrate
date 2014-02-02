module Data.String.Indent where
import Data.List

indent :: Int -> String -> String
indent amount string =
  let space s = replicate amount ' ' ++ s in
    intercalate "\n" $ map space $ lines string

indentFill :: String -> String -> String
indentFill fill original =
  intercalate "\n" (map (\s-> fill ++ s) $ lines original) ++ "\n\n"
