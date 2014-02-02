{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Data.Suite.Ops where
import qualified Data.List as L
import Data.String.Indent
import Control.Lens
import Data.Monoid
import qualified Data.Map as M
import Data.Integrated.TestModule

data Ops =
  Ops {
    _added :: Map, _removed :: Map, _modified :: Map, _unmodified :: Map
  }
$(makeLenses ''Ops)

empty :: Ops
empty = Ops mempty mempty mempty mempty

null :: Ops -> Bool
null ops =
  all (\f -> M.null . f $ ops) [_added, _removed, _modified, _unmodified]

instance Show Ops where
  show ops =
   L.intercalate "\n"
     (map (uncurry showMap) [
       ("added", view added ops)
       , ("removed", view removed ops)
       , ("modified", view modified ops)
       , ("unmodified", view unmodified ops)
       ]
   ) ++ "\n"
   where
     showMap :: String -> Map -> String
     showMap name_str m =
       if M.null m then
         "None " ++ name_str ++ "."
       else
         name_str ++ " with tests:\n" ++
           indent 1 (L.intercalate "\n" (map show (M.toList m)))
