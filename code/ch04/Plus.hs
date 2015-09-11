a `plus` b = a + b

data a `Pair` b = a `Pair` b deriving (Show)

-- we can use the constructor either prefix or infix
foo = Pair 1 2
bar = True `Pair` "quux"
