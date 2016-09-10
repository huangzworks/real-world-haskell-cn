-- file: ch25/D.hs
import System.Environment
import Text.Printf
import Data.List (fold')

main = do
  [d] <- map read `fmap` getArgs
  printf "%f\n" (mean [1..d])

mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    (n, s)     = foldl' k (0, 0) xs
    k (n, s) x = n `seq` s `seq` (n+1, s+x)
