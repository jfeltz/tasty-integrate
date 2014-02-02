-- TODO move to separate tile file generation package
module Test.Util where

-- import Filesystem.Path.CurrentOS
-- import Prelude hiding (FilePath, error)
-- import System.IO (Handle)
-- import System.IO.Util
-- import Data.Test

testFormat :: String -> String
testFormat s = "Qc" ++ s
--
-- testDir :: FilePath -> FilePath -> FilePath
-- testDir src_path test_dir = append test_dir (parent src_path)
--
--
-- testPath src_path test_dir =
--   let name = decodeString ( testFormat $ encodeString $ filename src_path) in
--     collapse $ append (testDir src_path test_dir) name
--
-- maybeTestHandle :: FilePath -> FilePath -> IO (Maybe Handle, FilePath)
-- maybeTestHandle src_path test_dir =
--   do
--     let p = testPath src_path test_dir
--     h <- maybeHandle p
--     maybe (return (Nothing, p)) (\h' -> return (Just h', p)) h
--
-- defaultPrefix :: String
-- defaultPrefix = "Qc"
--
-- -- revision :: Test -> Test -> Bool
-- -- revision a b = (a /= b) && testModpath a == testModpath b
