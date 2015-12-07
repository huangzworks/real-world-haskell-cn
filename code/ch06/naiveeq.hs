-- file: ch06/naiveeq.hs
import Data.Char (isSpace)
data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red   Red   = True
colorEq Green Green = True
colorEq Blue  Blue  = True
colorEq _     _     = False

stringEq :: [Char] -> [Char] -> Bool

-- Match if both are empty
stringEq [] [] = True

-- If both start with the same char, check the rest
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys

-- Everything else doesn't match
stringEq _ _ = False

-- instance Show Color where
--     show Red   = "Red"
--     show Green = "Green"
--     show Blue  = "Blue"

instance Show Color where
    show Red   = "Color 1: Red"
    show Green = "Color 2: Green"
    show Blue  = "Color 3: Blue"

-- instance Read Color where
--     -- readsPrec is the main function for parsing input
--     readsPrec _ value =
--         -- We pass tryParse a list of pairs.  Each pair has a string
--         -- and the desired return value.  tryParse will try to match
--         -- the input to one of these strings.
--         tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
--         where tryParse [] = []    -- If there is nothing left to try, fai
--               tryParse ((attempt, result):xs) =
--                     -- Compare the start of the string to be parsed to the
--                     -- text we are looking for.
--                   if (take (length attempt) value) == attempt
--                   -- If we have a match, return the result and the
--                   -- remaining input
--                   then [(result, drop (length attempt) value)]
--                   -- If we don't have a match, try the next pair
--                   -- in the list of attempts.
--                   else tryParse xs

instance Read Color where
    readsPrec _ value = tryParse colors
        where
          cleanedUpValue = dropWhile isSpace value
          tryParse [] = []
          tryParse ((attempt, result):xs) =
              if (take (length attempt) cleanedUpValue) == attempt
              then [(result, drop (length attempt) cleanedUpValue)]
              else tryParse xs
          colors = [("Red", Red), ("Green", Green), ("Blue", Blue)]
