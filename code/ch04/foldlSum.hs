-- file: ch04/foldlSum.hs
foldlSum xs = foldl step 0 xs
    where step acc x = acc + x
