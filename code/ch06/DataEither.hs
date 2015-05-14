-- file: ch06/DataEither.hs
data Maybe a = Nothing
             | Just a
               deriving (Eq, Ord, Read, Show)

data Either a b = Left a
                | Right b
                  deriving (Eq, Ord, Read, Show)