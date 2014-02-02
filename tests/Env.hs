module Env where
import Prelude hiding (FilePath)

data Env a = Env { setup :: IO a, teardown :: IO () }

runEnv :: Env a -> (a -> IO r) -> IO r
runEnv env operation = do
  identity <- setup env
  result <- operation identity
  teardown env
  return result
