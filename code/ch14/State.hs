-- file: ch14/State.hs
newtype State s a = State {
      runState :: s -> (a, s)
    }

-- file: ch14/State.hs
returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

-- file: ch14/State.hs
bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s'

-- file: ch14/State.hs
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
