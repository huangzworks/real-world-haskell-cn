-- file: ch14/MonadJoin.hs
join :: Monad m => m (m a) -> m a
join x = x >>= id
