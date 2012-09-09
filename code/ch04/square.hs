-- file: ch04/square.hs

square :: [Double] -> [Double]

square (x:xs) = x*x : square xs
square []     = []
