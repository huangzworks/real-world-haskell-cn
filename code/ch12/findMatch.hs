-- file: ch12/findMatch.hs
findMatch :: [(Run, Bit)] -> Maybe [[Digit]]
findMatch = listToMaybe
          . filter (not . null)
          . map (solve . candidateDigits)
          . tails
