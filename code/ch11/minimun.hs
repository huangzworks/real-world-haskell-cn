-- file: ch11/minimum.hs
head       :: [a] -> a
head (x:_) = x
head []    = error "Prelude.head: empty list"

minimum    :: (Ord a) => [a] -> a
minimum [] =  error "Prelude.minimum: empty list"
minimum xs =  foldl1 min xs