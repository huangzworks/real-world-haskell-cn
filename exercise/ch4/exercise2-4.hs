takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) = if f x
                      then x:(takeWhile' f xs)
                      else []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f = foldr step []
    where
        step x acc = if f x
                     then x : acc
                     else []