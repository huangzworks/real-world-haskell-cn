-- file: ch14/Logger.hs
globToRegex :: String -> Logger String

-- file: ch14/Logger.hs
module Logger
    (
      Logger
    , Log
    , runLogger
    , record
    ) where

-- file: ch14/Logger.hs
type Log = [String]

-- file: ch14/Logger.hs
runLogger :: Logger a -> (a, Log)

-- file: ch14/Logger.hs
record :: String -> Logger ()

-- file: ch14/Logger.hs
globToRegex cs =
    globToRegex' cs >>= \ds ->
    return ('^':ds)

-- file: ch14/Logger.hs
globToRegex' :: String -> Logger String
globToRegex' "" = return "$"

-- file: ch14/Logger.hs
globToRegex' ('?':cs) =
    record "any" >>
    globToRegex' cs >>= \ds ->
    return ('.':ds)

-- file: ch14/Logger.hs
globToRegex' ('*':cs) = do
    record "kleene star"
    ds <- globToRegex' cs
    return (".*" ++ ds)

-- file: ch14/Logger.hs
globToRegex' ('[':'!':c:cs) =
    record "character class, negative" >>
    charClass cs >>= \ds ->
    return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c : ds)
globToRegex' ('[':_) =
    fail "unterminated character class"

-- file: ch14/Logger.hs
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= \i ->
            return (f i)

-- file: ch14/Logger.hs
charClass_wordy (']':cs) =
    globToRegex' cs >>= \ds ->
    return (']':ds)
charClass_wordy (c:cs) =
    charClass_wordy cs >>= \ds ->
    return (c:ds)

-- file: ch14/Logger.hs
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs

-- file: ch14/Logger.hs
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

escape :: Char -> Logger String
escape c
    | c `elem` regexChars = record "escape" >> return ['\\',c]
    | otherwise           = return [c]
  where regexChars = "\\+()^$.{}]|"

-- file: ch14/Logger.hs
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 =
    m1 >>= \a ->
    m2 >>= \b ->
    return (f a b)

-- file: ch14/Logger.hs
newtype Logger a = Logger { execLogger :: (a, Log) }

-- file: ch14/Logger.hs
runLogger = execLogger

-- file: ch14/Logger.hs
record s = Logger ((), [s])

-- file: ch14/Logger.hs
instance Monad Logger where
    return a = Logger (a, [])

-- file: ch14/Logger.hs
    -- (>>=) :: Logger a -> (a -> Logger b) -> Logger b
    m >>= k = let (a, w) = execLogger m
                  n      = k a
                  (b, x) = execLogger n
              in Logger (b, w ++ x)
