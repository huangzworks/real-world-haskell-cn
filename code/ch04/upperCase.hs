-- file: ch04/upperCase.hs

import Data.Char (toUpper)

upperCase :: String -> String

upperCase (x: xs) = toUpper x : upperCase xs
upperCase []      = []
