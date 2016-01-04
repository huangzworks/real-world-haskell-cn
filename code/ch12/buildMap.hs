-- file: ch12/Barcode.hs
buildMap :: [[Parity Digit]] -> DigitMap
buildMap = M.mapKeys (realCheckDigit)
         . addFirstDigit
         . finalDigits
         	where realCheckDigit c = (10 - c) `mod` 10 