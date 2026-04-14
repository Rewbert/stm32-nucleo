module IxMonad where

import Control.Monad.IO.Class

-- indexed monad typeclass
class IxMonad m where
    ireturn :: a -> m p p a
    ibind :: m p q a -> (a -> m q r b) -> m p r b
    ilift :: IO a -> m p p a

(>>=) :: IxMonad m => m p q a -> (a -> m q r b) -> m p r b
ma >>= k = ibind ma k

(>>) :: IxMonad m => m p q a -> (m q r b) -> m p r b
ma >> k = ibind ma (const k)

return :: IxMonad m => a -> m p p a
return = ireturn