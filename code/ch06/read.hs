-- file: ch06/read.hs
main = do
  putStrLn "Please enter a Double:"
  inpStr <- getLine
  let inpDouble = (read inpStr)
  putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2.0))