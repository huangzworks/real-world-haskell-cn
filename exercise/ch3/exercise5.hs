isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [a] = True
isPalindrome (x:xs)
    | x == (last xs) = isPalindrome (init xs)
    | otherwise = False
