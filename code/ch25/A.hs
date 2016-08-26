-- file: ch25/A.hs
import System.Environment
import Text.Printf

main = do
    [d] <- map read `fmap` getArgs
    printf "%f\n" (mean [1..d])

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)
