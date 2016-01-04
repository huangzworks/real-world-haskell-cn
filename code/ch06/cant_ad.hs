-- file: ch06/cant_ad.hs
data Book = Book (Int -> Bool)
          deriving (Show)

data BookInfo = BookInfo Book
                deriving (Show)