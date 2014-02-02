module Data.Integrated.Partition where
import qualified Data.List as L
import Data.Integrated.TestModule (Map)
import qualified Data.Map as M

-- RETRO:
-- I'm curious if a OTS data structure is available to otherwise represent/enforce this
data Partition = Partition {
  equal :: Map, displacing :: Map, added :: Map
  }

-- RETRO:
-- I'm wondering if there is a templated solution to handle list-tuple boiler
-- plate types like the below
instance Show (Partition) where
  show p =
    L.intercalate "\n" $
      "partition:\n":
      map
        (\t -> fst t ++ " :  " ++ (show . M.toList . snd) t)
        [("equal", equal p), ("displacing", displacing p), ("added", added p)]
