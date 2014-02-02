-- FIXME reconcile issues with reports
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-binds -fno-warn-unused-do-bind  #-}
{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs hiding (ignore)
import System.Environment
import Control.Monad.Trans.Either

import Control.Monad.IO.Class

import qualified System.Console.CmdArgs.Explicit as Exp
import qualified Data.List as L
import qualified Data.Suite as S

import qualified Data.Integrated as I
import qualified Data.Integrated.IOState as IOS
import Control.Monad.State
import Control.Lens
import qualified Filesystem.Path.CurrentOS as FS
import Prelude hiding (FilePath)
import Data.Aeson hiding (json)
import Data.AesonData
import Data.Suite.Ops
import Data.Integrated.TestModule (Map)
import qualified Data.Map as M
import Data.ModulePath
import Data.Property
import Data.String.Indent
import qualified Data.Suite as Su
import qualified Data.ByteString.Lazy.Char8 as BS

data Args =
  Args {
    json :: Maybe String,
    masking :: Bool,
    ignore :: Bool,
    suite :: String,
    tests :: [String]
  } deriving (Show, Data, Typeable)

integration =
  let
    description =
      "This program set-combines quickcheck property-containing modules with "
      ++ "an IDE parsable test harness."
  in
  Args {
    json =
      def
        -- &= typ "json file path"
        &= help "generate a report of operations on the target suite in JSON"
        &= opt ""
    , masking =
      def &= help "keep existing tests in suite not part of sync"
    , ignore =
      def &= help "ignore test file parse failures (only parsed will be set-combined)"
    , suite =
        def &= typ "<suite file path>" &= argPos 0
    , tests =
        [] &= args &= typ "files/dirs"
  } &= help "The tasty suite file integrator"
    &= program "tasty-integrate"
    &= summary "tasty-integrate v0.0 (C) John P. Feltz"
    &= details [description]
    &= verbosity

fromArgs :: Args -> EitherT String IO I.Integrated
fromArgs a =
  if (L.null . tests) a then
    left "no tests provided"
  else do
    fs_state <- liftIO $ IOS.toState (tests a) (suite a)
    integrated <-
      liftIO $ do
        (maybe_integrated, errors) <- runStateT (ranState fs_state) []
        putStr $ L.intercalate "\n" $ map show errors
        return maybe_integrated
    maybe (left "\n\nHalted due to errors.") right integrated
  where
    fromIgnore :: Bool -> I.ErrorObservance
    fromIgnore True = I.Ignored
    fromIgnore _    = I.Dependent

    ranState = I.toIntegrated (fromIgnore $ ignore a) (masking a)

fromIntegrated :: Maybe Verbosity -> Maybe String -> I.Integrated -> IO ()
fromIntegrated v j i = do
  ops <- I.fromIntegrated (S.toBuf S.tastyImports) i
  let (suite_fp, original_suite) = view I.suiteFile i
  -- Print the summary
  putStrLn ('\n': toSummary original_suite suite_fp ops v)
  -- Write json to console or fileput if necessary
  maybe (return ()) (writeJson suite_fp ops) j
  where
    writeJson :: FS.FilePath -> Ops -> String -> IO ()
    writeJson suite_fp ops p =
      if L.null p then putStr "\n" >> putStrLn encoded else writeFile p encoded
      where
        encoded :: String
        encoded = BS.unpack . encode $ AesonData ops suite_fp

fromProps :: [Property] -> String
fromProps []       = "none"
fromProps non_empty = L.intercalate "\n" . map func $ non_empty

toModuleListing :: Map -> String
toModuleListing m =
  if M.null m then " none" else
    L.intercalate "\n" . map (uncurry fromMod) . M.toList . M.map snd $ m
  where
    fromMod :: ModulePath -> [Property] -> String
    fromMod mp props = show mp ++ '\n':indent 1 (fromProps props)

toNumericListing :: Map -> String
toNumericListing m =
  if M.null m then
    "none"
  else
    indent 1
      (L.intercalate "\n" . map (uncurry fromMod) . M.toList . M.map snd $ m)
    where
      fromMod :: ModulePath -> [Property] -> String
      fromMod mp props =
        show mp ++ ", " ++ (show . L.length $ props) ++ " property(s)"

toSummary :: Su.Suite -> FS.FilePath -> Ops -> Maybe Verbosity -> String
toSummary _ _  _   (Just Quiet) = ""
toSummary s fp ops (Just Loud)  = fromOriginal toModuleListing s fp ops ++ "\n"
toSummary s fp ops _            = fromOriginal toNumericListing s fp ops ++ "\n"

fromOps :: (Map -> String) -> Ops -> String
fromOps f ops =
  L.intercalate "\n" $
    map (\(label, l) -> label ++ ": \n" ++ indent 1 (f . view l $ ops)) [
       ("added",      added),
       ("removed",    removed),
       ("modified",   modified),
       ("unmodified", unmodified)
     ]

fromOriginal :: (Map -> String) -> Su.Suite -> FS.FilePath -> Ops -> String
fromOriginal f Su.Uncreated suite_fp ops =
  if not . M.null $ view added ops then
    "(created) suite at: " ++ FS.encodeString suite_fp ++ "\n"
    ++ "with: \n" ++ indent 1 (f (view added ops))
  else
    "no suite created"
fromOriginal f (Su.Suite _) suite_fp  ops =
  if mutation then
    "(changed) suite at: " ++ FS.encodeString suite_fp ++ "\n"
    ++ "with: \n" ++ indent 1 (fromOps f ops)
  else
    "(unchanged) suite at: " ++ FS.encodeString suite_fp
  where
    mutation =
     any (not . M.null . ($ ops) . view) [added, removed, modified]

main :: IO ()
main = do
  env_args <- getArgs

  case Exp.process (cmdArgsMode integration) env_args of
    Right parsed_args ->
      case cmdArgsHelp parsed_args of
        (Just h) -> putStr h
        Nothing  ->
          let
              program_args = cmdArgsValue parsed_args
              v =  cmdArgsVerbosity parsed_args
          in do
            result <- runEitherT . fromArgs $ program_args
            case result of
              Left  e ->
                dispatchHelp e
              Right i ->
                fromIntegrated v (json program_args) i
    Left err  -> dispatchHelp err
  where
    dispatchHelp :: String -> IO ()
    dispatchHelp err = putStr $ err ++ "\n " ++ "(--help for assistance)\n"
