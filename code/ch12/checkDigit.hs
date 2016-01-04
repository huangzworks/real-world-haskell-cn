-- file: ch12/Barcode.hs
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = productSum `mod` 10 `mod` 10
    where productSum = sum products (mapEveryOther (*3) (reverse ds))

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f,id])