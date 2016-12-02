intersperse :: a -> [[a]] -> [a]
intersperse _ [x] = x
intersperse sep (x:xs) = x ++ [sep] ++ (intersperse sep xs)