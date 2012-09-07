-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)

-- file: ch03/BookStore.hs
data MagzineInfo = Magzine Int String [String]
                   deriving (Show)

-- file: ch03/BookStore.hs
myInfo = Book 9780135072455 "Algebra of Programming"
              ["Richard Bird", "Oege de Moor"]
