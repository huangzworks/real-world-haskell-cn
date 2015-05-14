-- file: ch11/Arbitrary.hs

module Arbitrary where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen


-- file: ch11/Arbitrary.hs
data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq,Show)

-- file: ch11/Arbitrary.hs
instance Arbitrary Ternary where
    arbitrary     = elements [Yes, No, Unknown]

-- file: ch11/Arbitrary2.hs
instance Arbitrary Ternary where
    arbitrary     = do
        n <- choose (0, 2) :: Gen Int
        return $ case n of
                      0 -> Yes
                      1 -> No
                      _ -> Unknown

-- file: ch11/Arbitrary.hs
instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (x, y)