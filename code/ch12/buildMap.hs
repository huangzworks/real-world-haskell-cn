-- file: ch12/buildMap.hs
buildMap :: [[Parity Digit]] -> DigitMap
buildMap = M.mapKeys (10 -)
         . addFirstDigit
         . finalDigits