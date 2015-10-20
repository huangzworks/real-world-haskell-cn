-- file: ch12/Barcode.hs
updateMap :: Parity Digit       -- ^ new digit
          -> Digit              -- ^ existing key
          -> [Parity Digit]     -- ^ existing digit sequence
          -> ParityMap          -- ^ map to update
          -> ParityMap
updateMap digit key seq = insertMap key (fromParity digit) (digit:seq)

insertMap :: Digit -> Digit -> [a] -> Map a -> Map a
insertMap key digit val m = val `seq` M.insert key' val m
    where key' = (key + digit) `mod` 10