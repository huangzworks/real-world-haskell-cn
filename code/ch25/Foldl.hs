-- file: ch25/Foldl.hs
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z xs = lgo z xs
  where lgo z []     = z
        lgo z (x:xs) = let z' = f z x in z' `seq` lgo z' xs
