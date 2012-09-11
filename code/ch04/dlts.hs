-- file: ch04/dlts.hs

import Data.List (isPrefixOf)

dlts :: String -> [String]

dlts = foldr step [] . lines
  where step l ds
          | "#define DLT_" `isPrefixOf` l = secondWord l : ds
          | otherwise                     = ds
        secondWord = head . tail . words
