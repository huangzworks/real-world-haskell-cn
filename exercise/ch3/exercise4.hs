palindrome :: [a] -> [a]
palindrome [] = []
palindrome s = s ++ reverse s