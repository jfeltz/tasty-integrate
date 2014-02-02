{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.QcIntegrated where
import Data.Integrated hiding (tests)
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Monadic

import Test.QuickCheck.Property.Comb

import qualified Data.Property as P
import qualified Data.Suite.Ops as O
import Control.Lens hiding (lens, mapping)
import Control.Applicative
import Data.Integrated.Partition
import Data.Integrated.TestModule
import Control.Monad.State

import Util.Invariants
import Data.String.Builder
import Data.ModulePath hiding (toPath)
import Data.Path

import Arbitrary.Properties
import Arbitrary.TestModule (toGenerated)
import Arbitrary.FS

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.List as L

import Prelude hiding (FilePath)

import System.Directory
import Env

main :: IO ()
main = void $quickCheckAll

prop_toPartition :: Diff -> Property
prop_toPartition diff =
  runInvariants
    (diff, toPartition diff)
    inv_toPartition
  where
    -- TODO/FIXME cleanup remove diff type
    inv_toPartition :: Invariants (Diff, Partition)
    inv_toPartition = do
      satcomp (\t -> (incoming . fst $ t, snd t)) $
        eqCoverageInv
          "union of [equal, displacing, added] form improper subset of incoming"
          [equal, displacing, added]
      satcomp (fromPart . snd) disjointSetsInv
      satcomp (\t -> (fst t, added . snd $ t)) addedInv
      satcomp (\t -> (incoming . fst $ t, displacing . snd $ t)) displacingInv
      satcomp (\t -> (fst t, equal . snd $ t)) equalInv
      where
        fromPart p =
          toSets p [
            ("equal",      S.fromList . M.keys . equal),
            ("displacing", S.fromList . M.keys . displacing),
            ("added",      S.fromList . M.keys . added)
            ]

        eqCoverageInv ::
          String ->
          [Partition -> Map] ->
          Invariants (Map, Partition)
        eqCoverageInv msg fs =
          sat $ do
            (s, p) <- cause
            doc $ msg ++ "\n\n"
            doc $ show p
            return $ (M.unions . map ($ p)) fs == s

        addedInv :: Invariants (Diff, Map)
        addedInv =
          sat $ do
            d <- fst <$> cause
            doc "added are the modulepath-set difference of incoming minus original"
            (==) (M.difference (incoming d) (original d)) . snd <$> cause

        -- TODO members of displacing either differ by property content,
        -- by file-path, or both
        displacingInv :: Invariants (Map, Map)
        displacingInv =
          sat $ do
            incoming_map <- fst <$> cause
            doc "displacing is a submap of incoming"
            (`M.isSubmapOf` incoming_map) . snd <$> cause

        equalInv :: Invariants (Diff, Map)
        equalInv =
          sat $ do
            (d, equal_map) <- cause
            doc "equal is a submap of original and incoming"
            doc $ "equal: " ++ show equal_map
            return $
              (equal_map `M.isSubmapOf` incoming d)
                && (equal_map `M.isSubmapOf` original d)

instance Arbitrary Diff where
  arbitrary = fromOriginal =<< fromMask M.empty =<< choose (0, 4)
    where
      -- | Produce a list of added sets, or sets that don't intersect on
      -- modulepath with any in the original set.
      fromMask :: Map -> Int -> Gen Map
      fromMask mask 0 =
        return mask
      fromMask mask n = do
        (p, satisfying_tm) <- maskSatisfying
        fromMask
          (M.insert
            (view modpath satisfying_tm)
            (p, view properties satisfying_tm)
            mask
            )
          (n-1)
        where
          -- Note the suthThat predicate relies upon comparison of
          -- test module path, and this isn't very clear from
          -- appearances
          maskSatisfying :: Gen (FS.FilePath, TestModule)
          maskSatisfying =
            suchThat
              (toGenerated (choose ('a','z')) (M.keysSet mask))
                -- =<< toModulePath (choose ('a', 'z')))
              (\(_, tm) -> not . S.member (view modpath tm) $ M.keysSet mask)
      fromOriginal :: Map -> Gen Diff
      fromOriginal m = do
        intersecting <- choose (0, M.size m)
        equivalent   <- choose (0, intersecting)
        changed      <- choose (0, intersecting - equivalent)
        Diff m <$> toIncoming (M.toList m) equivalent changed
        where
          toIncoming ::
            [(ModulePath, (FS.FilePath, [P.Property]))]
            -> Int
            -> Int
            -> Gen Map
          toIncoming original_list equivalent changed =
            -- Partition the original into equivalent and changed and
            -- exclude the rest
            let
              equivalent_map = M.fromList . take equivalent $ original_list
            in do
              added_total <- choose (0,2)
              changed_map <- genChangedMap
              M.unions . (:[equivalent_map, changed_map]) <$>
                fromMask (equivalent_map `M.union` changed_map) added_total
            where
              genChangedMap =
                let subseq = take changed . drop equivalent $ original_list in
                do
                  fs <- vectorOf (L.length subseq) mutation
                  return . M.fromList $ zipWith (\f p -> over _2 f p) fs subseq
              -- Note, I could have used QuickCheck.Function here, but
              -- that is really overkill
              mutation ::
                Gen ((FS.FilePath, [P.Property]) -> (FS.FilePath, [P.Property]))
              mutation =
                oneof $ map return [over _1 pathMutation, over _2 propMutation]
                where
                  pathMutation :: FS.FilePath -> FS.FilePath
                  pathMutation p =
                    FS.decodeString $ FS.encodeString "z/" ++ FS.encodeString p

                  propMutation :: [P.Property] -> [P.Property]
                  propMutation []       = [toProperty 0]
                  propMutation (_:rest) = rest

data Input_fromPartition =
  Input_fromPartition {
    masking :: Bool, original_map :: Map, partition :: Partition
  } deriving Show

instance Arbitrary Input_fromPartition where
   arbitrary = do
     bool <- arbitrary :: Gen Bool
     diff <- suchThat (arbitrary :: Gen Diff) (not . M.null . original)
     return $ Input_fromPartition bool (original diff) (toPartition diff)

-- TODO/FIXME cleanup/remove input type
prop_fromPartition :: Input_fromPartition -> Property
prop_fromPartition input =
  let
    result =
      runState
        (fromPartition (masking input) (partition input)) (original_map input)
  in
    runInvariants (result, input) inv_fromPartition
  where
    -- Note by precondition,
    --   all invariants on the Partition are held (disjoint, relations with
    -- imported etc).
    inv_fromPartition :: Invariants ((O.Ops, Map), Input_fromPartition)
    inv_fromPartition = do
      ops_invariants
      satcomp (\((_, ss), i) -> (ss,i)) suite_invariants
      where
        ops_invariants :: Invariants ((O.Ops, Map), Input_fromPartition)
        ops_invariants =
          let
            fromOpsLens lens t = set (_1 . _1) (view (_1 . _1 . lens) t) t
            addedRelations t =
              ((view (_1 . _1 . O.added) t, view (_1 . _2) t),
               partition . snd $ t
              )
            removedRelations t =
              ((view (_1 . _1 . O.removed) t, view (_1 . _2) t), snd t)
          in do
            satcomp (toLabelled . fst . fst) disjointSetsInv
            satcomp addedRelations ops_added
            satcomp removedRelations ops_removed
            satcomp (fromOpsLens O.modified) ops_modified
            satcomp (fromOpsLens O.unmodified) ops_unmodified

        ops_added :: Invariants ((Map, Map), Partition)
        ops_added = do
          satcomp (\t -> (fst . fst $ t, added . snd $ t)) . sat $ do
            doc "added is equivalent to added in partition"
            uncurry (==) <$> cause
          satcomp fst . sat $ do
            doc "added is a submap of the suite mapping"
            uncurry M.isSubmapOf <$> cause

        toLabelled :: O.Ops -> [LabelledSet ModulePath]
        toLabelled ops =
          toSets ops
            [ ("added",      S.fromList . M.keys . O._added)
             ,("removed",    S.fromList . M.keys . O._removed)
             ,("modified",   S.fromList . M.keys . O._modified)
             ,("unmodified", S.fromList . M.keys . O._unmodified)
             ]

        ops_removed :: Invariants ((Map, Map), Input_fromPartition)
        ops_removed = do
          sat $ do
            doc "removed is a subset of the original suite set"
            ((removed_map, _), i) <- cause
            return $ M.isSubmapOf removed_map (original_map i)
          satcomp fst . sat $ do
            doc "non-empty removed is not a subset of the final suite set"
            (removed_map, final_suite_map) <- cause
            return . (if M.null removed_map then id else not) $
              M.isSubmapOf removed_map final_suite_map
          sat $ do
            ((removed_map, _), i) <- cause
            if masking i
              then do
                doc . build $ do
                  "masking => removed is the difference: "
                  "  original - (equal of partition U displaced) "
                return $
                  (==)
                    removed_map
                    (M.difference
                      (original_map i)
                      (displaced `M.union` (equal . partition $ i))
                    )
              else do
                doc "not masking => removed empty"
                return . M.null $ removed_map
              where
                displaced =
                  M.intersection
                    (original_map input)
                    (displacing . partition $ input)

        ops_modified :: Invariants ((Map, Map), Input_fromPartition)
        ops_modified = do
          sat $ do
            doc "modified is a submap of original"
            ((modified_map, _), i) <- cause
            return $ M.isSubmapOf modified_map (original_map i)
          sat $ do
            doc . build $ do
              "modified is domain-equivalent to displacing of partition,"
              "and for every equal relation (d1,d2) in a domain pairing,"
              "s.t d1 -> v1 and d2 -> v2, v1 neq v2"
            ((modified_map, _), i) <- cause
            let displacing_map = displacing . partition $ i
                domain_eq   = M.null $ M.difference modified_map displacing_map
            return . (&&) domain_eq
                   . and . zipWith (/=) (M.elems modified_map)
                       $ M.elems displacing_map

        ops_unmodified :: Invariants ((Map, Map), Input_fromPartition)
        ops_unmodified = do
          sat $ do
            doc "unmodified is a submap of original"
            ((unmodified_map, _), i) <- cause
            return $ M.isSubmapOf unmodified_map (original_map i)
          sat $ do
            ((unmodified_map, _), i) <- cause
            if masking i
              then do
                doc "masking => unmodified is only the equal set"
                return $ unmodified_map == (equal . partition $ i)
              else do
                doc "not masking => original - displaced-mapped of partition"
                return $
                  unmodified_map ==
                    M.difference
                      (original_map i)
                      ( M.intersection
                        (original_map i)
                        (displacing . partition $ i)
                      )

        suite_invariants :: Invariants (Map, Input_fromPartition)
        suite_invariants =
          sat $ do
            (final_suite, _) <- cause
            if masking input
              then do
                doc "the final suite is (equal U added U displacing)"
                return $ final_suite == minimum_expected
              else do
                doc . build $ do
                  "the final suite is latent U (equal U added U displacing)"
                  " where latent = original_set - displaced"
                  " where displaced = domain mapped via displacing"
                return $ final_suite == M.union latent minimum_expected
              where
                latent = M.difference (original_map input) displaced
                displaced =
                  M.intersection
                    (original_map input)
                    (displacing . partition $ input)
                minimum_expected =
                  M.unions $ map ($ partition input) [equal, displacing, added]

test_isomorphism :: Invariants (Tests, Tests)
test_isomorphism =
  sat $ do
    doc "parsed tests are equal to those on filesystem"
    (written, parsed) <- cause
    if parsed /= written
      then do
        doc (msg parsed written)
        doc $ "\n written tests: \n" ++ (show . M.toList $ written)
        doc $ "\n parsed tests: \n" ++ (show . M.toList $ parsed)
        return False
      else return True
    where
      msg parsed written =
        if M.size parsed > M.size written then
          fromDifference "\n excessive parsed:\n " parsed written
        else
          fromDifference "\n unparsed:\n " written parsed
      fromDifference des l r = des ++ (show . M.toList $ M.difference l r)

errors_isomorphism :: Invariants (Errors, Errors)
errors_isomorphism =
  sat $ do
    doc "parsed errors are equal to those on filesystem"
    (written, parsed) <- cause
    if parsed /= written
      then do
        doc (msg parsed written)
        doc $ "\n written: \n" ++ (show . S.toList $ written)
        doc $ "\n parsed: \n" ++ (show . S.toList $ parsed)
        return False
      else return True
    where
      msg parsed written =
        if S.size parsed > S.size written then
          fromDifference "\n excessive parsed:\n " parsed written
        else
          fromDifference "\n unparsed:\n " written parsed
      fromDifference des l r =
        des ++ (show . S.toList $ S.difference l r)

type Tests = M.Map ModulePath [String]
type Errors = S.Set FS.FilePath

-- FS is a Set of file-path(s), so it is sufficient to check for
-- isomorphisms on the two path-disjoint sets forming its union,
-- test files and non-sense files

fromTestpathInv :: FS.FilePath -> Invariants (FS, (Map, [Error]))
fromTestpathInv sandbox = do
  satcomp (uncurry to_test_isomorphism) test_isomorphism
  satcomp (uncurry to_errors_isomorphism) errors_isomorphism
  where
    to_test_isomorphism :: FS -> (Map, [Error]) -> (Tests, Tests)
    to_test_isomorphism fs (parsed_tests, _) =
      (,)
        (M.fromList
          . L.map (\tm -> (view modpath tm, map P.func $ view properties tm))
          . M.elems
          . fst
          . M.mapEither id
          $ mapping fs) -- Written
        (M.map (map P.func . snd) parsed_tests)

    to_errors_isomorphism :: FS -> (Map, [Error]) -> (Errors, Errors)
    to_errors_isomorphism fs (_, parsed_errors)   =
      (,)
        (S.map (FS.append sandbox)
         . M.keysSet
         . snd
         . M.mapEither id $
         mapping fs) -- Given
        (S.fromList . map filepath $ parsed_errors) -- parsed

-- FIXME/TODO still need to populate the directory with tests
env :: String -> FS -> Env FS.FilePath
env sandbox fs =
  Env {
    setup = do -- TODO add deleted state after completed
      createDirectory sandbox
      writeFS decoded_path fs
      return decoded_path
    , teardown    = do
        removeDirectoryRecursive sandbox
        return ()
    } where decoded_path = FS.decodeString sandbox

filterIndicatives :: FS -> FS
filterIndicatives =
  FS
    . M.filterWithKey (\k _ -> legalModSubpath . FS.encodeString $ k)
    . mapping

prop_fromTestpath_isomorphism :: FS -> Property
prop_fromTestpath_isomorphism fs =
  let sandbox = "./tmp/" in
    monadicIO $ do
      isomorphic <- run $ runEnv (env sandbox fs) toIsomorphic
      stop $
        runInvariants
          (filterIndicatives fs, isomorphic)
          (fromTestpathInv $ FS.decodeString sandbox)
  where
    toIsomorphic :: FS.FilePath -> IO (Map, [Error])
    toIsomorphic fp = do
      path  <- toPath $ FS.encodeString fp
      runStateT (fromTestpath path) []
