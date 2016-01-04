-- file: ch14/ListMonad.hs
returnSingleton :: a -> [a]
returnSingleton x = [x]

-- file: ch14/ListMonad.hs
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)

-- file: ch14/ListMonad.hs
    xs >> f = concat (map (\_ -> f) xs)
    fail _ = []

-- file: ch14/ListMonad.hs
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
