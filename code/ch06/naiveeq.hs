-- file: ch06/naiveeq.hs
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
