-- TODO verbosity options
-- FIXME atm this parser is VERY brittle, please email me if there are any
-- issues -J
module Main where
import System.IO
import System.Posix.IO
import Data.List
import Data.Either
import Text.Parsec.String (Parser)
import Prelude hiding (fail)
import Control.Applicative ((<*>),(<$>))
import Data.String.Indent (indent)

import Text.Parsec.Prim (parse, try, (<|>), many)
import Text.Parsec.Combinator (optionMaybe, count, many1, choice)
import Text.Parsec.Char (alphaNum, char, string, noneOf)
import Text.Parsec.Extra ( natural, eol, whitespace )
import qualified Filesystem.Path.CurrentOS as FS
import Prelude hiding (FilePath)

data Success = Success { successMsg :: String } deriving Show
data Failure = Failure { line :: Int, failureMsg :: String } deriving Show
data Test = Test { test :: String, result :: Either Failure Success }
              deriving Show
data File = File { filepath :: FS.FilePath, tests :: [Test] } deriving Show

fromResult :: FS.FilePath -> Test -> String
fromResult fp (Test t (Left (Failure l msg))) =
  FS.encodeString fp ++ '|':show l ++ " error | " ++ t ++ '\n':indent 1 msg
fromResult _  (Test t (Right (Success msg))) =
  t ++ "\n " ++ msg

fromFile :: File -> String
fromFile f =
  let (failures, successes) = partition failed (tests f) in
    (++) (intercalate "\n" $ map toString failures)
         ('\n': fromSuccesses successes)
  where
    failed :: Test -> Bool
    failed (Test _ (Left (Failure _ _))) = True
    failed (Test _ (Right (Success _)))  = False

    toString :: Test -> String
    toString = fromResult (filepath f)

    fromSuccesses :: [Test] -> String
    fromSuccesses []   = []
    fromSuccesses list = FS.encodeString (filepath f) ++  " successes: \n" ++
        indent 2 (concatMap toString list)

pathLine :: Parser FS.FilePath
pathLine = do
  count 2 whitespace
  non_ext <- many1 $ noneOf ".\SP\n\r"
  ext     <- string ".hs"
  eol
  return . FS.decodeString $ non_ext ++ ext

messages :: Parser [String]
messages = do
  count 6 whitespace
  content <- many $ noneOf "\n\r"
  many1 eol
  rest <- optionMaybe (try messages)
  return $ maybe [content] (content:) rest

testP :: Parser Test
testP = do
  count 4 whitespace
  l <- natural
  char ':'
  whitespace
  string "prop_"
  prop_name <- many1 (alphaNum <|> char '_')
  char ':'
  many1 whitespace ; f <- status prop_name l ; eol
  f . unlines <$> messages
  where
    status :: String -> Int -> Parser (String -> Test)
    status prop_name l = do
      lit <- choice $ map string ["OK", "FAIL"]
      return $
        Test prop_name .
          if lit == "OK" then Right . Success else Left . Failure l

parser :: Parser [File]
parser = do
  string "modules"
  eol
  many1 (File <$> pathLine <*> many1 (try testP))

parseResults :: String -> String
parseResults runner_output =
 case parse parser "tasty consoleRunner output" runner_output of
    Left  errors -> show errors
    Right files  -> (intercalate "\n" . map fromFile $ files) ++ "\n"

main :: IO ()
main = fdToHandle stdInput >>= hGetContents >>= putStr . parseResults
