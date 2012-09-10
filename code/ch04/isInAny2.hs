-- file: ch04/isInAny2.hs

import Data.List (isInfixOf)

isInAny2 needle haystack = any (\s -> needle `isInfixOf` s) haystack
