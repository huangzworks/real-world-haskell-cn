-- file: ch19/toupper-impch20.hs
import System.IO
import System.IO.Error
import Data.Char(toUpper)

main :: IO ()
main = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do input <- try (hGetLine inh)
       case input of
         Left e -> 
             if isEOFError e
                then return ()
                else ioError e
         Right inpStr ->
             do hPutStrLn outh (map toUpper inpStr)
                mainloop inh outh
