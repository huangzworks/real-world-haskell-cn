import System.Environment
import Text.Printf
import Control.Parallel.Strategies


main = do
  [d] <- map read `fmap` getArgs
  printf "%f\n" (mean [1..d])

fold'rnf :: NFData a => (a -> b -> a) -> a -> [b] -> a
fold'rnf f z xs = lgo z xs
  where
    lgo z [] = z
    lgo z (x:xs) = lgo z' xs
      where
        z' = f z x `using` rnf

mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    (n, s) = fold'rnf k (0, 0) xs
    k (n, s) x = (n+1, s+x) :: (Int, Double)





