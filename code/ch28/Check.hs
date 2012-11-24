-----------------------------------------------------------------------------
-- |
-- Module      :  UrlCheck.hs
-- Copyright   :  (c) Don Stewart 2006
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | 译者注：
--   Mon Nov 12 22:58:19 2012
--   这个代码在原作者的基础做了一点修改，在以下环境下编译通过
--     + GHC-7.4.1
--     + HTTP-4000.2.5
-----------------------------------------------------------------------------


{-- snippet top --}
{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving,
             PatternGuards #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (catch, finally, IOException)
import Control.Monad.Error
import Control.Monad.State
import Data.Char (isControl)
import Data.List (nub)
import Network.URI
import Prelude hiding (catch)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Set as S

-- This requires the HTTP package, which is not bundled with GHC
import Network.HTTP hiding (Done)
import Network.Stream

type URL = B.ByteString

data Task = Check URL | Done
{-- /snippet top --}

{-- snippet main --}
main :: IO ()
main = do
    (files,k) <- parseArgs
    let n = length files

    -- count of broken links
    badCount <- newTVarIO (0 :: Int)

    -- for reporting broken links
    badLinks <- newTChanIO

    -- for sending jobs to workers
    jobs <- newTChanIO

    -- the number of workers currently running
    workers <- newTVarIO k

    -- one thread reports bad links to stdout
    forkIO $ writeBadLinks badLinks

    -- start worker threads
    forkTimes k workers (worker badLinks jobs badCount)

    -- read links from files, and enqueue them as jobs
    stats <- execJob (mapM_ checkURLs files)
                     (JobState S.empty 0 jobs)

    -- enqueue "please finish" messages
    atomically $ replicateM_ k (writeTChan jobs Done)

    waitFor workers

    broken <- atomically $ readTVar badCount

    printf fmt broken
               (linksFound stats)
               (S.size (linksSeen stats))
               n
  where
    fmt   = "Found %d broken links. " ++
            "Checked %d links (%d unique) in %d files.\n"
{-- /snippet main --}

{-- snippet modifyTVar_ --}
modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ tv f = readTVar tv >>= writeTVar tv . f

forkTimes :: Int -> TVar Int -> IO () -> IO ()
forkTimes k alive act =
  replicateM_ k . forkIO $
    act
    `finally`
    (atomically $ modifyTVar_ alive (subtract 1))
{-- /snippet modifyTVar_ --}

{-- snippet writeBadLinks --}
writeBadLinks :: TChan String -> IO ()
writeBadLinks c =
  forever $
    atomically (readTChan c) >>= putStrLn >> hFlush stdout
{-- /snippet writeBadLinks --}

{-- snippet waitFor --}
waitFor :: TVar Int -> IO ()
waitFor alive = atomically $ do
  count <- readTVar alive
  check (count == 0)
{-- /snippet waitFor --}

{-- snippet getStatus --}
getStatus :: URI -> IO (Either String Int)
getStatus = chase (5 :: Int)
  where
    chase 0 _ = bail "too many redirects"
    chase n u = do
      resp <- getHead u
      case resp of
        Left err -> bail (show err)
        Right r ->
          case rspCode r of
            (3,_,_) ->
               case findHeader HdrLocation r of
                 Nothing -> bail (show r)
                 Just u' ->
                   case parseURI u' of
                     Nothing -> bail "bad URL"
                     Just url -> chase (n-1) url
            (a,b,c) -> return . Right $ a * 100 + b * 10 + c
    bail = return . Left

getHead :: URI -> IO (Result (Response String))
getHead uri = simpleHTTP Request { rqURI = uri,
                                   rqMethod = HEAD,
                                   rqHeaders = [],
                                   rqBody = "" }
{-- /snippet getStatus --}

{-- snippet getStatusE --}
getStatusE = runErrorT . chase (5 :: Int)
  where
    chase :: Int -> URI -> ErrorT String IO Int
    chase 0 _ = throwError "too many redirects"
    chase n u = do
      r <- embedEither show =<< liftIO (getHead u)
      case rspCode r of
        (3,_,_) -> do
            u'  <- embedMaybe (show r)  $ findHeader HdrLocation r
            url <- embedMaybe "bad URL" $ parseURI u'
            chase (n-1) url
        (a,b,c) -> return $ a*100 + b*10 + c

-- This function is defined in Control.Arrow.
left :: (a -> c) -> Either a b -> Either c b
left f (Left x)  = Left (f x)
left _ (Right x) = Right x

-- Some handy embedding functions.
embedEither :: (MonadError e m) => (s -> e) -> Either s a -> m a
embedEither f = either (throwError . f) return

embedMaybe :: (MonadError e m) => e -> Maybe a -> m a
embedMaybe err = maybe (throwError err) return
{-- /snippet getStatusE --}

{-- snippet worker --}
worker :: TChan String -> TChan Task -> TVar Int -> IO ()
worker badLinks jobQueue badCount = loop
  where
    -- Consume jobs until we are told to exit.
    loop = do
        job <- atomically $ readTChan jobQueue
        case job of
            Done  -> return ()
            Check x -> checkOne (B.unpack x) >> loop

    -- Check a single link.
    checkOne url = case parseURI url of
        Just uri -> do
            code <- getStatus uri `catch` wrapException -- (return . Left . show) 
            case code of
                Right 200 -> return ()
                Right n   -> report (show n)
                Left err  -> report err
        _ -> report "invalid URL"

        where report s = atomically $ do
                           modifyTVar_ badCount (+1)
                           writeTChan badLinks (url ++ " " ++ s)
              wrapException :: IOException -> IO (Either String Int)
              wrapException = return . Left . show
              
{-- /snippet worker --}

{-- snippet Job --}
data JobState = JobState { linksSeen :: S.Set URL,
                           linksFound :: Int,
                           linkQueue :: TChan Task }

newtype Job a = Job { runJob :: StateT JobState IO a }
    deriving (Monad, MonadState JobState, MonadIO)

execJob :: Job a -> JobState -> IO JobState
execJob = execStateT . runJob
{-- /snippet Job --}

{-- snippet checkURLs --}
checkURLs :: FilePath -> Job ()
checkURLs f = do
    src <- liftIO $ B.readFile f
    let urls = extractLinks src
    filterM seenURI urls >>= sendJobs
    updateStats (length urls)

updateStats :: Int -> Job ()
updateStats a = modify $ \s ->
    s { linksFound = linksFound s + a }

-- | Add a link to the set we have seen.
insertURI :: URL -> Job ()
insertURI c = modify $ \s ->
    s { linksSeen = S.insert c (linksSeen s) }

-- | If we have seen a link, return False.  Otherwise, record that we
-- have seen it, and return True.
seenURI :: URL -> Job Bool
seenURI url = do
    seen <- (not . S.member url) `liftM` gets linksSeen
    insertURI url
    return seen

sendJobs :: [URL] -> Job ()
sendJobs js = do
    c <- gets linkQueue
    liftIO . atomically $ mapM_ (writeTChan c . Check) js
{-- /snippet checkURLs --}

{-- snippet extractLinks --}
extractLinks :: B.ByteString -> [URL]
extractLinks = concatMap uris . B.lines
  where uris s      = filter looksOkay (B.splitWith isDelim s)
        isDelim c   = isControl c || c `elem` " <>\"{}|\\^[]`"
        looksOkay s = http `B.isPrefixOf` s
        http        = B.pack "http:"
{-- /snippet extractLinks --}

{-- snippet parseArgs --}
data Flag = Help | N Int
            deriving Eq

parseArgs :: IO ([String], Int)
parseArgs = do
    argv <- getArgs
    case parse argv of
        ([], files, [])                     -> return (nub files, 16)
        (opts, files, [])
            | Help `elem` opts              -> help
            | [N n] <- filter (/=Help) opts -> return (nub files, n)
        (_,_,errs)                          -> die errs
  where
    parse argv = getOpt Permute options argv
    header     = "Usage: urlcheck [-h] [-n n] [file ...]"
    info       = usageInfo header options
    dump       = hPutStrLn stderr
    die errs   = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
    help       = dump info                  >> exitWith ExitSuccess
{-- /snippet parseArgs --}

{-- snippet options --}
options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)
                   "Show this help message",
            Option ['n'] []       (ReqArg (\s -> N (read s)) "N")
                   "Number of concurrent connections (default 16)" ]
{-- /snippet options --}
