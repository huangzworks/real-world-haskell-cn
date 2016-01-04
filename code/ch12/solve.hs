-- file: ch12/Barcode.hs
solve :: [[Parity Digit]] -> [[Digit]]
solve [] = []
solve xs = catMaybes $ map (addCheckDigit m) checkDigits
    where checkDigits = map fromParity (last xs)
          m = buildMap (init xs)
          addCheckDigit m k = (++[k]) <$> M.lookup k m