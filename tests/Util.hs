{-# LANGUAGE ScopedTypeVariables #-}
module Util where
import Test.QuickCheck.Property.Comb
import Data.Set
import qualified Data.List as L
import Control.Applicative
import Prelude hiding (pred, null)

type LabelledSet a = (String, Set a)

disjoint :: forall a. (Ord a) => Invariants [LabelledSet a]
disjoint =
  sat $ do
    non_disjoint <- L.filter (not . snd) . L.map (uncurry pred) . toPairings <$> cause
    doc . L.intercalate "\n" . L.map fst $ non_disjoint
    return $ L.null non_disjoint
  where
    -- TODO Remove (a, b), (b, a) from the subsequences
    toPairings :: [LabelledSet a] -> [(LabelledSet a, LabelledSet a)]
    toPairings =
      L.map (\l-> (head l, l !! 1))
        . L.filter (\l -> length l == 2)
        . L.subsequences
    pred :: LabelledSet a -> LabelledSet a -> (String, Bool)
    pred (desc1, s1) (desc2, s2) =
      (,)
        (desc1 ++ " and " ++ desc2 ++ " disjoint ")
        (null $ intersection s1 s2)
