-- file: ch04/strictFoldl.hs

foldl' _ zero [] = zero
foldl' step zero (x:xs) = 
    let new = step zero x
    in new `seq` foldl' step new xs
