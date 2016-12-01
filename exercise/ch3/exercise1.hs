myLength :: [a] -> Int
myLength = iter 0 where
    iter ans [] = ans
    iter ans (x:xs) = iter (ans+1) xs