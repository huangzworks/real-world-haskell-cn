-- file: ch15/MonadPlus.hs
    mzero >>= f == mzero

-- file: ch15/MonadPlus.hs
    v >> mzero == mzero    

-- file: ch15/MonadPlus.hs
guard        :: (MonadPlus m) => Bool -> m ()
guard True   =  return ()
guard False  =  mzero

-- file: ch15/MonadPlus.hs
x `zeroMod` n = guard ((x `mod` n) == 0) >> return x