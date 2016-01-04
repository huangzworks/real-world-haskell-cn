-- file: ch14/Random.hs
import System.Random

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

-- file: ch14/Random.hs
twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)

-- file: ch14/Random.hs
twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = let (a, gen') = random gen
                         (b, gen'') = random gen'
                     in ((a, b), gen'')

-- file: ch14/Random.hs
type RandomState a = State StdGen a

-- file: ch14/Random.hs
getRandom :: Random a => RandomState a
getRandom =
  get >>= \gen ->
  let (val, gen') = random gen in
  put gen' >>
  return val

-- file: ch14/Random.hs
getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom

-- file: ch14/Random.hs
runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  oldState <- getStdGen
  let (result, newState) = runState getTwoRandoms oldState
  setStdGen newState
  return result

-- file: ch14/Random.hs
data CountedRandom = CountedRandom {
      crGen :: StdGen
    , crCount :: Int
    }

type CRState = State CountedRandom

getCountedRandom :: Random a => CRState a
getCountedRandom = do
  st <- get
  let (val, gen') = random (crGen st)
  put CountedRandom { crGen = gen', crCount = crCount st + 1 }
  return val

-- file: ch14/Random.hs
getCount :: CRState Int
getCount = crCount `liftM` get

-- file: ch14/Random.hs
putCount :: Int -> CRState ()
putCount a = do
  st <- get
  put st { crCount = a }

-- file: ch14/Random.hs
putCountModify :: Int -> CRState ()
putCountModify a = modify $ \st -> st { crCount = a }
