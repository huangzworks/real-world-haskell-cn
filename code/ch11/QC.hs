-- file: ch11/QC.hs
module QC where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad(liftM, liftM2)
import Data.List(intersperse)
import Prettify2


-- file: ch11/QC.hs
instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]

-- file: ch11/QC.hs
prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

-- file: ch11/QC.hs

prop_char c   = char c   == Char c

prop_text s   = text s   == if null s then Empty else Text s

prop_line     = line     == Line

prop_double d = double d == Text (show d)

-- file: ch11/QC.hs

prop_hcat xs = hcat xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds

-- file: ch11/QC.hs

prop_punctuate s xs = punctuate s xs == intersperse s xs

-- file: ch11/QC.hs
prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine []           = []
        combine [x]          = [x]

        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys

-- file: ch11/QC.hs
prop_mempty_id x =
    mempty `mappend` x == x
  &&
    x `mappend` mempty == (x :: Doc)