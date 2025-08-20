module NonSecure where

import Foreign.Storable
import Foreign.Ptr
import Control.Monad.State
import Data.IORef

import Setup

-- * Secure monad, a monad for describing secure computation

data Secure a = SecureDummy

instance Functor Secure where
    fmap _ SecureDummy = SecureDummy

instance Applicative Secure where
    pure _ = SecureDummy

    SecureDummy <*> SecureDummy = SecureDummy

instance Monad Secure where
    SecureDummy >>= _ = SecureDummy

instance Storable a => Storable (Secure a) where
    sizeOf :: Storable a => Secure a -> Int
    sizeOf _ = error "cannot use Storable on non secure"

    alignment :: Storable a => Secure a -> Int
    alignment _ = error "cannot use Storable on non secure"

    peek :: Storable a => Ptr (Secure a) -> IO (Secure a)
    peek ptr = error "cannot use Storable on non secure"

    poke = error "cannot use Storable on non secure"

-- * Embedding NonSecureCallable functions

class NonSecureCallable a where
    mkNSC :: a -> ([String] -> Secure String)

instance (Show a, Read a) => NonSecureCallable (Secure a) where
    mkNSC v = \_ -> fmap show v

instance (Show a, Read a, NonSecureCallable b) => NonSecureCallable (a -> b) where
    mkNSC f = \(x:xs) -> mkNSC (f $ read x) xs

-- * API for the user to designate functions as NSC, and applying them

data Callable a = Callable Int [String]

callable :: (NonSecureCallable a) => a -> Setup (Callable a)
callable _ = do
    setupst <- get
    put $ setupst { counter = counter setupst + 1 }
    return $ Callable (counter setupst) []

(<.>) :: (Show a, Read a) => Callable (a -> b) -> a -> Callable b
Callable id xs <.> x = Callable id (show x : xs)

-- * Secure references, to enable mutable state in the secure world

data SRef a = SRefDummy

initialSRef :: a -> Setup (Secure (SRef a))
initialSRef a = return $ newSRef a

newSRef :: a -> Secure (SRef a)
newSRef _ = SecureDummy

readSRef :: SRef a -> Secure a
readSRef _ = SecureDummy

writeSRef :: SRef a -> a -> Secure ()
writeSRef _ _ = SecureDummy

modifySRef :: SRef a -> (a -> a) -> Secure ()
modifySRef ref f = do
    v <- readSRef ref
    writeSRef ref (f v)

-- * Invoking non secure callable function in the secure world

sg :: (Show a, Read a) => Callable (Secure a) -> IO a
sg _ = return $ error "TODO: here is where we actually invoke the trustzone, shipping this closure and retrieving the result"

nonSecure :: IO a -> Setup ()
nonSecure ns = do
    v <- liftIO ns
    return $ v `seq` ()

runSetup :: Setup () -> IO ()
runSetup _ = undefined