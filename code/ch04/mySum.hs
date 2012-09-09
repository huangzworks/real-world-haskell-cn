-- file: ch04/mySum.hs

mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc []     = acc
