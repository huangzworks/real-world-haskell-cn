any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr step False
  where
    step x acc = f x || acc

cycle' :: [a] -> [a]
cycle' [] = []
cycle' s = foldr (\x acc-> s++acc) [] [1..]

words' :: String -> [String]
words' [] = []
words' s = filter (/= " ") . groupBy' f $ s
  where
    f x y = not (x == ' ' || y == ' ')

unlines' :: [String] -> String
unlines' [] = []
unlines' s = tail . foldr (\x acc->'\n':x++acc) "\n" $ s