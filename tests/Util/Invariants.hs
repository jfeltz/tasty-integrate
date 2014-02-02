{-# LANGUAGE ScopedTypeVariables #-}
module Util.Invariants where
import Test.QuickCheck.Property.Comb
import Data.Set
import Control.Applicative
import Prelude hiding (pred, null)
import qualified Data.Set as S
import qualified Data.List as L

type LabelledSet a = (String, Set a)

toSets :: a -> [(String, a -> S.Set b)] -> [LabelledSet b]
toSets set_container =
  let toSet name f = (name, f set_container) in L.map (uncurry toSet)

disjointSetsInv :: forall a. (Ord a) => Invariants [LabelledSet a]
disjointSetsInv =
  sat $ do
    non_disjoint <-
      L.filter (not . snd) . L.map (uncurry pred) . toPairings <$> cause
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
