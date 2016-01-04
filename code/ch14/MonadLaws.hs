-- file: ch14/MonadLaws.hs
fmap id        ==   id 
fmap (f . g)   ==   fmap f . fmap g

-- file: ch14/MonadLaws.hs
return x >>= f            ===   f x

-- file: ch14/MonadLaws.hs
do y <- return x
   f y                    ===   f x

-- file: ch14/MonadLaws.hs
m >>= return              ===   m

-- file: ch14/MonadLaws.hs
do y <- m
   return y               ===   m

-- file: ch14/MonadLaws.hs
m >>= (\x -> f x >>= g)   ===   (m >>= f) >>= g

-- file: ch14/MonadLaws.hs
m >>= s
  where s x = f x >>= g

-- file: ch14/MonadLaws.hs
t >>= g
  where t = m >>= f

-- file: ch14/MonadLaws.hs
m >>= s                   ===   t >>= g
