-- file: ch12/Barcode.hs
distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b