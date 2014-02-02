module Arbitrary.Properties where
import Test.QuickCheck hiding (Property)
import Data.Property

data Properties = Properties { list :: [Property] } deriving Show

toProperty :: Int ->  Property
toProperty count = Property 0 ("prop_" ++ show count)

instance Arbitrary Properties where
  arbitrary = do
    numProperties <- choose (0, 2)
    return . Properties . map toProperty $ [1..numProperties]
