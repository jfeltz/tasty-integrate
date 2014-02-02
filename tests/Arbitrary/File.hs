module Arbitrary.File where
import Test.QuickCheck
--import qualified Data.Test as T
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath, writeFile)
import qualified Data.Set as S
import Control.Applicative
import Data.ModulePath
import Control.Lens

import Arbitrary.TestModule (toGenerated, testModulePath)
import Arbitrary.FilePath
import Arbitrary.Properties
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc (noLoc)
import Data.Integrated.TestModule

import qualified Data.List as L
import qualified Data.Property as P
import Control.Monad.State

isTest :: Content -> Bool
isTest (Left _) = True
isTest _        = False

type Nonsense = String
type Content = Either TestModule Nonsense

data File = File { path :: FilePath, content :: Content }
instance Show File where
  show (File p (Left t))  = show p ++ ": \n" ++ show t
  show (File p (Right n)) = show p ++ ": \n" ++ show n

toStr :: TestModule -> String
toStr tm =
  prettyPrint (toModule [] $ view modpath tm) ++ '\n' : append_properties_buf
  where
    append_properties_buf :: String
    append_properties_buf =
      L.intercalate "\n" .  map (\f -> P.func f ++ " = undefined") $
        view properties tm

-- Note, Nonsense can contain erroneously test data
-- or have a test like path with erroneous data

nonSenseGen :: Gen Char -> S.Set FilePath -> Gen File
nonSenseGen subpath avoided =
  (\(p,b) -> File p (Right b)) <$>
    frequency [ (1, testModule), (2, fakeModule), (2, nonModule) ]
  where
    testModule,fakeModule,nonModule :: Gen (FilePath, String)
    testModule = do -- This case should fail gracefully.
      mp  <- testModulePath subpath S.empty
      buf <- oneof [ garbageBuf, testBuf mp ]
      return (relPath mp, buf)
    fakeModule = do
      mp <- suchThat arbitrary (not . flip S.member avoided . relPath)
      buf <- oneof [ testBuf mp, moduleBuf mp ]
      return (relPath mp, buf)
    nonModule = liftM2 (,) (filepathGen subpath) garbageBuf

    moduleBuf,testBuf :: ModulePath -> Gen String
    testBuf mp = toStr . TestModule mp . list <$> (arbitrary :: Gen Properties)
    moduleBuf mp =
      return . prettyPrint $
        Module noLoc (ModuleName $ show mp) [] Nothing Nothing [] []

    garbageBuf :: Gen String
    garbageBuf = return "Nonsense"

fileGen :: Gen Char -> StateT (S.Set FilePath, S.Set ModulePath) Gen File
fileGen subpath = do
  test <- lift $ choose (True, False)
  if test
    then do
      avoided <- snd <$> get
      (fp, tm) <- lift $ toGenerated subpath avoided
      modify
        (\t -> (S.insert fp . fst $ t, S.insert (view modpath tm) . snd $ t))
      return $ File fp (Left tm)
    else do
      avoided <- fst <$> get
      f <- lift $ nonSenseGen subpath avoided
      modify (over _1 (S.insert (path f)))
      return f
