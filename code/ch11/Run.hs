-- file: ch11/Run.hs
module Main where
import QC
import Test.QuickCheck

anal :: Args
anal = Args
    { replay = Nothing
    , maxSuccess = 1000
    , maxDiscardRatio = 1
    , maxSize = 1000
    , chatty = True
    }

minimal :: Args
minimal = Args
    { replay = Nothing
    , maxSuccess = 200
    , maxDiscardRatio = 1
    , maxSize = 200
    , chatty = True
    }

runTests :: Args -> IO ()
runTests args = do
    f prop_empty_id "empty_id ok?"
    f prop_char "char ok?"
    f prop_text "text ok?"
    f prop_line "line ok?"
    f prop_double "double ok?"
    f prop_hcat "hcat ok?"
    f prop_punctuate' "punctuate ok?"
    f prop_mempty_id "prop_mempty_id ok?"
    where
        f prop str = do
            putStrLn str
            quickCheckWithResult args prop
            return ()

main :: IO ()
main = do
    putStrLn "Choose test depth"
    putStrLn "1. Anal"
    putStrLn "2. Minimal"
    depth <- readLn
    if depth == 1
        then runTests anal
    else runTests minimal