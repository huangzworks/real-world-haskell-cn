import Data.Char (digitToInt)
fI = fromIntegral

asInt :: String -> Float
asInt [] = 0
asInt ('-':xs) = - (asInt xs)
asInt s
    | '.' `elem` s = fI (parsePositive ints) + (parsePoint . tail $ floats)
    | otherwise = fI (parsePositive ints)
    where
        (ints, floats) = break (=='.') s
        parsePositive :: String -> Int
        parsePositive = foldl (\acc x -> acc * 10 + digitToInt x) 0
        parsePoint :: String -> Float
        parsePoint = foldr (\x acc -> acc/10.0 + (fI . digitToInt $ x)/10.0) 0.0
