-- file: ch12/distance.hs
distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b