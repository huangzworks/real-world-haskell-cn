-- file: ch15/MonadHandle.hs
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module MonadHandle (MonadHandle(..)) where

import System.IO (IOMode(..))

class Monad m => MonadHandle h m | m -> h where
    openFile :: FilePath -> IOMode -> m h
    hPutStr :: h -> String -> m ()
    hClose :: h -> m ()
    hGetContents :: h -> m String

    hPutStrLn :: h -> String -> m ()
    hPutStrLn h s = hPutStr h s >> hPutStr h "\n"
