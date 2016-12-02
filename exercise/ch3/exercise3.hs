fI = fromIntegral

mean :: [Int] -> Float
mean xs = (fI . sum $ xs) / (fI . myLength $ xs)