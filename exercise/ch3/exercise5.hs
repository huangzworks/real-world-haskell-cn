isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:xs)
    | x == (last xs) = isPalindrome (init xs)
    | otherwise = False