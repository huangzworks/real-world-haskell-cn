-- file: ch06/JAry.hs
newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show)