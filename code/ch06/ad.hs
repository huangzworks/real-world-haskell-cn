-- file: ch06/ad.hs
data Book = Book
            deriving (Show)

data BookInfo = BookInfo Book
                deriving (Show)
                         