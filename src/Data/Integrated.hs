-- FIXME
-- 1. There is a legitimate case for loading the tests into the suite on
-- memory, and skipping file-system altogether (TODO). This
-- is a feature slated for 2.0, which will decouple the suite framework
-- used.
--
-- 2. producing an empty suite file which returns a succesful test run case
-- is disasterously misleading for CI, this case must be assured not to
-- occur

{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Integrated where
import Control.Lens
import Prelude hiding (FilePath)
import Control.Monad.State
import Data.Path
import Data.Integrated.TestModule (Map)
import qualified Data.Integrated.TestModule as TM
import Data.Suite
import Control.Applicative
import System.IO.Util

import Data.Integrated.IOState
import Data.Integrated.Partition

import qualified Data.List as L
import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.Map as M
import qualified Data.Suite.Ops as O
import Data.Monoid (mempty, Monoid)
import Data.ModulePath

data Diff = Diff { original :: Map, incoming :: Map }

instance Show Diff where
  show diff =
    L.intercalate "\n" $
      "diff:\n":
      map
        (\t -> fst t ++ " :  " ++ (show . M.toList . snd) t)
        [("original", original diff), ("incoming", incoming diff)]

-- This represents a complete read of the file system, prior to any future
-- writes.
data Integrated =
  Integrated {
    _masked :: Bool, _tests :: Map, _suiteFile :: (FS.FilePath, Suite)
    } deriving Show
$(makeLenses ''Integrated)

data Error =
  Error { filepath :: FS.FilePath, description :: String } deriving (Ord, Eq)
instance Show Error where show (Error p d) = FS.encodeString p ++ ": " ++ d

-- TODO, MaybeT could have been added to this stack, but I don't see the
-- value of this extra type constraint atm
type ErrorsM r = StateT [Error] IO r

appendError :: Error -> ErrorsM ()
appendError e = modify (e:)

-- Note this holds the following post-conditions WRT filesystem:
--  for every erroneous indicative test, and error is produced,
--  if no tests were found, the mapping is empty.
fromTestpath :: Path -> ErrorsM Map
fromTestpath (NotExisting p)                    = do
  appendError $
    Error (FS.decodeString p) " test path failed to exist"
  return mempty
fromTestpath (Existing (File p))                = do
  buf <- liftIO . readFile . FS.encodeString $ p
  either fromError (return . fromTestModule) $ TM.fromBuf buf
  where
    fromError ::  (Monoid r) => String ->  ErrorsM r
    fromError e = appendError (Error p e) >> return mempty
    fromTestModule :: TM.TestModule -> Map
    fromTestModule tm =
      M.singleton (view TM.modpath tm) (p, view TM.properties tm)
fromTestpath (Existing (Directory p)) = do
  leaves <- liftIO $ do
    -- For this case, a file-path not indicative of a Test is discarded
    speculative_tests <- L.filter haskellModule <$> toDirectoryLeafs p
    mapM (Data.Path.toPath . FS.encodeString) speculative_tests
  M.unions <$> mapM fromTestpath leaves
  where
    haskellModule = legalModSubpath . FS.encodeString

-- Note this holds the following post-conditions WRT filesystem:
--  If Nothing, this is an erroneous path, or suite file on filesystem
--  Otherwise the suite is not created, or is encoded on file-system
fromSuitepath :: Path -> ErrorsM (Maybe (FS.FilePath, Suite))
fromSuitepath (Existing (File p)) =
  let str_p = FS.encodeString p in do
    buf <- liftIO $ readFile str_p
    case Data.Suite.fromBuf str_p buf of
      Left error_msg -> do
        appendError $
          Error p error_msg
        appendError $
          Error p "please correct, or delete and regenerate suite file"
        return Nothing
      Right m -> return . Just $ (p, Suite m)
fromSuitepath (Existing (Directory p)) = do
  appendError $ Error p "it's a directory."
  return Nothing
fromSuitepath (NotExisting str) =
  return . Just $ (FS.decodeString str, Uncreated)

-- Note, if the file-path of the same module is different, this is
-- effectively the same change as a change of property

-- | Take two possibly intersecting test sets and disjoint partition them
-- as defined by the type.
toPartition :: Diff -> Partition
toPartition d =
  Partition {
    equal        = equal'
    , displacing = M.difference left_intersection equal'
    , added      = M.difference (incoming d) (original d)
    }
  where
    equal' :: Map
    equal' =
      M.fromList
        -- Take the incoming side of the equality
        . map (\(k, (v1, _)) -> (k, v1))
        . L.filter (\(_, (v1, v2)) -> v1 == v2)
        -- Pair the values for each equal module-path
        $ zipWith (\(k, v1) v2 -> (k, (v1, v2)))
           (M.toList left_intersection)
           (map snd $ M.toList right_intersection)

    left_intersection :: Map
    left_intersection = M.intersection (incoming d) (original d)

    right_intersection :: Map
    right_intersection = M.intersection (original d) (incoming d)

data ErrorObservance = Dependent | Ignored

toIntegrated :: ErrorObservance -> Bool -> IOState -> ErrorsM (Maybe Integrated)
toIntegrated error_obs masking (IOState test_paths suite_path) =
  maybe (return Nothing) (uncurry fromSuite) =<< fromSuitepath suite_path
  where
    fromSuite :: FS.FilePath -> Suite -> ErrorsM (Maybe Integrated)
    fromSuite p s = do
      test_mappings <- M.unions <$> mapM fromTestpath test_paths
      errors <- get
      return $
        case error_obs of
          Dependent ->
            if not . L.null $ errors then
              Nothing
            else
              Just $ Integrated masking test_mappings (p, s)
          Ignored   ->
            Just $ Integrated masking test_mappings (p, s)

toOperations :: Bool -> Map -> State Suite O.Ops
toOperations masking incoming_map = do
  previous <- get
  case previous of
    (Suite original_map) ->
      let
        (ops, s) =
          flip runState original_map . fromPartition masking $
            toPartition $ Diff original_map incoming_map
      in do
        put $ Suite s
        return ops
    Uncreated ->
      if M.null incoming_map then
        return O.empty
      else do
        put (Suite incoming_map)
        return $ set O.added incoming_map O.empty

-- REMARK: In hindsight, I'm not sure if pre-meditated use of the State
-- monad is appropriate, I could just as easily created:
--  Bool -> Partition -> Map -> Map
--  Bool -> Partition -> Map -> Ops
--  and tested them faster, at the moment the only direct-payoff seems to be
--  in code legibility/conciseness, at the expense of reasoning

fromPartition :: Bool -> Partition -> State Map O.Ops
fromPartition masking partition = do
  original_map <- get
  let displaced =
        M.intersection original_map (displacing partition)
      added_and_modified =
        O.empty { O._added = added partition, O._modified = displaced }
      minimum_suite =
        M.unions . map ($ partition) $ [equal, displacing, added]
  if masking
    then
      let
        removed :: Map
        removed =
          M.difference original_map (equal partition `M.union` displaced)
      in do
        put minimum_suite
        return $
          added_and_modified {
            O._removed    = removed,
            O._unmodified =
              M.difference original_map (displaced `M.union` removed)
            }
    else -- Retain unmodified and non-equal-to-incoming tests in the suite
      do
        put $
          M.union minimum_suite
            (M.difference original_map (displaced `M.union` equal partition))
        return $
          added_and_modified {
            O._unmodified = M.difference original_map displaced
          }

fromIntegrated :: (Map -> String) -> Integrated -> IO O.Ops
fromIntegrated to_suite_buf integrated =
  let
    (ops, s) =
      runState
        (toOperations (_masked integrated) (_tests integrated))
        (snd . _suiteFile $ integrated)
  in do
    case s of
      (Suite test_mapping) -> writeFile (FS.encodeString fp) buffer where
        buffer = to_suite_buf test_mapping
        fp     = fst $ _suiteFile integrated
      _                    -> return ()
    return ops
