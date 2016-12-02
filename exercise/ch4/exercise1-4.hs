import Data.List (transpose)

transpose' :: String -> String
transpose' = unlines . transpose . lines