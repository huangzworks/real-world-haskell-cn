-- file: ch15/SupplyInstance.hs
newtype Reader e a = R { runReader :: e -> a }

-- file: ch15/SupplyInstance.hs
instance Monad (Reader e) where
    return a = R $ \_ -> a
    m >>= k = R $ \r -> runReader (k (runReader m r)) r

-- file: ch15/SupplyInstance.hs
ask :: Reader e e
ask = R id

-- file: ch15/SupplyInstance.hs
newtype MySupply e a = MySupply { runMySupply :: Reader e a }
    deriving (Monad)

instance MonadSupply e (MySupply e) where
    next = MySupply $ do
             v <- ask
             return (Just v)

    -- more concise:
    -- next = MySupply (Just `liftM` ask)

-- file: ch15/SupplyInstance.hs
xy :: (Num s, MonadSupply s m) => m s
xy = do
  Just x <- next
  Just y <- next
  return (x * y)

-- file: ch15/SupplyInstance.hs
runMS :: MySupply i a -> i -> a
runMS = runReader . runMySupply