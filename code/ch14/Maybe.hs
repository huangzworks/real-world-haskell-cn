-- file: ch14/Maybe.hs
data Maybe a = Nothing
             | Just a

-- file: ch14/Maybe.hs
chain :: m a -> (a -> m b) -> m b

-- file: ch14/Maybe.hs
inject :: a -> m a

-- file: ch14/Maybe.hs
class Monad m where
    -- chain
    (>>=)  :: m a -> (a -> m b) -> m b
    -- inject
    return :: a -> m a

-- file: ch14/Maybe.hs
    (>>) :: m a -> m b -> m b
    a >> f = a >>= \_ -> f

-- file: ch14/Maybe.hs
    fail :: String -> m a
    fail = error

-- file: ch14/Maybe.hs
instance Monad Maybe where
    Just x >>= k  =  k x
    Nothing >>= _ =  Nothing

    Just _ >> k   =  k
    Nothing >> _  =  Nothing

    return x      =  Just x

    fail _        =  Nothing

-- file: ch14/Maybe.hs
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x
