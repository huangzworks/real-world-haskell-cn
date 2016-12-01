concat' :: [[a]] -> [a]
concat' = foldr (++) []