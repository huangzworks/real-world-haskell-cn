-- file: ch15/Supply.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.State

module Supply
    (
      Supply
    , next
    , runSupply
    ) where

newtype Supply s a = S (State [s] a) deriving (Monad)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

next :: Supply s (Maybe s)
next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)