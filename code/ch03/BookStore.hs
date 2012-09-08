-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)

-- file: ch03/BookStore.hs
data MagzineInfo = Magzine Int String [String]
                   deriving (Show)

-- file: ch03/BookStore.hs
myInfo = Book 9780135072455 "Algebra of Programming"
              ["Richard Bird", "Oege de Moor"]

-- file: ch03/BookStore.hs
-- 稍后就会介绍 CustomerID 的定义

data BookReview = BookReview BookInfo CustomerID String

-- file: ch03/BookStore.hs
type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

-- file: ch03/BookStore.hs
type BookRecord = (BookInfo, BookReview)

-- file: ch03/BookStore.hs
type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- file: ch03/BookStore.hs
bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors
