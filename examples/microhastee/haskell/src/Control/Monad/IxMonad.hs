module Control.Monad.IxMonad where

import Control.Monad.IO.Class

-- indexed monad typeclass
class IxMonad m where
    ireturn :: a -> m p q p q a
    ibind :: m p q p2 q2 a -> (a -> m p2 q2 p3 q3 b) -> m p q p3 q3 b
    ilift :: IO a -> m p q p q a

(>>=) :: IxMonad m => m p q p2 q2 a -> (a -> m p2 q2 p3 q3 b) -> m p q p3 q3 b
ma >>= k = ibind ma k

(>>) :: IxMonad m => m p q p2 q2 a -> (m p2 q2 p3 q3 b) -> m p q p3 q3 b
ma >> k = ibind ma (const k)

return :: IxMonad m => a -> m p q p q a
return = ireturn