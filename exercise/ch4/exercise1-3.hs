firstWord :: String -> String
firstWord [] = []
firstWord s = unlines . map (head . words) $ (lines s)
