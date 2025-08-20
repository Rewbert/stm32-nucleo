module Secure where

import Foreign.Storable
import Foreign.Ptr
import Control.Monad.State
import Data.IORef
import Data.Proxy
import Unsafe.Coerce

import Setup

-- * Secure monad, a monad for describing secure computation

newtype Secure a = Secure (IO a)

instance Functor Secure where
    fmap f (Secure ioa) = Secure $ f <$> ioa

instance Applicative Secure where
    pure x = Secure $ pure x

    Secure iof <*> Secure ioa = Secure (iof <*> ioa)

instance Monad Secure where
    Secure ioma >>= k = Secure $ do
        a <- ioma
        let Secure b = k a
        b

-- | We will pretend that we store actual secure values, but we actually only need to be
-- able to read storable values from the heap and reconstruct them as Secure (return -val-).
--
-- We never need to store a secure value. We only want to be able to assign the return type
-- of our foreign functions as Secure, so that the type checker disallows the normal world
-- from invoking them
--
-- To do this, when we read values from the heap we just refer to the underlying values
-- storable instance.
instance Storable a => Storable (Secure a) where
    sizeOf :: Storable a => Secure a -> Int
    sizeOf _ = sizeOf (error "do not evaluate" :: a)

    alignment :: Storable a => Secure a -> Int
    alignment _ = alignment (error "do not evaluate" :: a)

    peek :: Storable a => Ptr (Secure a) -> IO (Secure a)
    peek ptr = do
        v <- peek (unsafeCoerce ptr :: Ptr a)
        return $ Secure $ return v

    poke = error "should never poke Secure"

-- * Embedding NonSecureCallable functions

class NonSecureCallable a where
    mkNSC :: a -> ([String] -> Secure String)

instance (Show a, Read a) => NonSecureCallable (Secure a) where
    mkNSC v = \_ -> fmap show v

instance (Show a, Read a, NonSecureCallable b) => NonSecureCallable (a -> b) where
    mkNSC f = \(x:xs) -> mkNSC (f $ read x) xs

-- * API for the user to designate functions as NSC, and applying them

data Callable a = CallableDummy

callable :: (NonSecureCallable a) => a -> Setup (Callable a)
callable f = do
    let g :: [String] -> IO String
        g bs = let Secure n = mkNSC f bs
               in n

    modify $ \st ->
        st { counter = counter st + 1
           , nonSecureCallable = (counter st, g) : nonSecureCallable st
           }

    return CallableDummy

(<.>) :: (Show a, Read a) => Callable (a -> b) -> a -> Callable b
(<.>) = error "the secure world cannot call the non secure world"

-- * Secure references, to enable mutable state in the secure world

type SRef a = IORef a

initialSRef :: a -> Setup (Secure (SRef a))
initialSRef a = return $ newSRef a

newSRef :: a -> Secure (SRef a)
newSRef a = Secure $ newIORef a

readSRef :: SRef a -> Secure a
readSRef ref = Secure $ readIORef ref

writeSRef :: SRef a -> a -> Secure ()
writeSRef ref a = Secure $ writeIORef ref a

modifySRef :: SRef a -> (a -> a) -> Secure ()
modifySRef ref f = do
    v <- readSRef ref
    writeSRef ref (f v)

-- * Invoking non secure callable function in the secure world

sg :: (Show a, Read a) => Callable (Secure a) -> IO a
sg _ = return $ error "the server should never execute this function"

nonSecure :: IO a -> Setup ()
nonSecure _ = return ()

runSetup :: Setup () -> IO ()
runSetup _ = undefined