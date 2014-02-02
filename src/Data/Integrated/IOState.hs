{-# LANGUAGE TemplateHaskell #-}
module Data.Integrated.IOState where
import Control.Lens
import Data.Path
import Control.Monad

data IOState = IOState { _paths :: [Path], _suite :: Path }
$(makeLenses ''IOState)

toState :: [String] -> String -> IO IOState
toState encoded_test_paths encoded_suite_path =
  liftM2 IOState (mapM toPath encoded_test_paths) (toPath encoded_suite_path)
