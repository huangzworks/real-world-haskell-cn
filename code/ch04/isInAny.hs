-- file: ch04/isInAny.hs

import Data.List (isInfixOf)

isInAny needle haystack = any inSequence haystack
    where inSequence s = needle `isInfixOf` s
