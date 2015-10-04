-- file: ch12/Barcode.hs
runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . runLength
