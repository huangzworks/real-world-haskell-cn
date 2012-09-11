-- file: ch04/suffixes.hs

import Data.List (tails)

suffixes2 xs = init (tails xs)
