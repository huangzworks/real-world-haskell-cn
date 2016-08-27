-- file: ch25/G.hs
import System.Environment
import Text.Printf
import Data.List (foldl')

main = do
  [d] <- map read `fmap` getArgs
  printf "%f\n" (mean [1..d])

data Pair a b = Pair !a !b

mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    Pair n s = foldl' k (Pair 0 0) xs
    k (Pair n s) x = Pair (n+1) (s+x)
