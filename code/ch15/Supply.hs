-- file: ch15/Supply.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply
    (
      Supply
    , next
    , runSupply
    ) where

import Control.Monad.State

runSupply :: Supply s a -> [s] -> (a, [s])
next :: Supply s (Maybe s)

newtype Supply s a = S (State [s] a) deriving (Functor, Applicative, Monad)

runSupply (S m) xs = runState m xs

next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

-- file: ch15/Supply.hs
showTwo :: (Show s) => Supply s String
showTwo = do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ ", b: " ++ show b)