-- file: ch14/AltMonad.hs
import Prelude hiding ((>>=), return)

class Functor m => AltMonad m where
    join :: m (m a) -> m a
    return :: a -> m a

(>>=) :: AltMonad m => m a -> (a -> m b) -> m b
xs >>= f = join (fmap f xs)
