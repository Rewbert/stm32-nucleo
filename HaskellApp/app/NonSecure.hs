{-# LANGUAGE InstanceSigs #-}
module NonSecure where

import Foreign.Storable
import Foreign.Ptr
import Control.Monad.State
import Control.Monad.IO.Class

import Setup

import Foreign.C.String (CString, withCString, peekCStringLen)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (allocaBytes, alloca)
import Foreign.Storable (peek)

foreign import ccall "sg.h sg" c_sg :: CString -> CString -> Ptr CInt -> IO ()

-- * Secure monad, a monad for describing secure computation

data Secure a = SecureDummy

instance Functor Secure where
    fmap _ SecureDummy = SecureDummy

instance Applicative Secure where
    pure _ = SecureDummy

    SecureDummy <*> SecureDummy = SecureDummy

instance Monad Secure where
    SecureDummy >>= _ = SecureDummy

unsafeLiftIO :: IO a -> Secure a
unsafeLiftIO _ = SecureDummy

-- * Embedding NonSecureCallable functions

class NonSecureCallable a where
    mkNSC :: a -> ([String] -> Secure String)

instance (Show a, Read a) => NonSecureCallable (Secure a) where
    mkNSC v = \_ -> fmap show v

instance (Show a, Read a, NonSecureCallable b) => NonSecureCallable (a -> b) where
    mkNSC f = \list -> case list of
                         [] -> error "cannot have empty list here"
                         (x:xs) -> mkNSC (f $ read x) xs

-- * API for the user to designate functions as NSC, and applying them

data Callable a = Callable Int [String]

callable :: (NonSecureCallable a) => a -> Setup (Callable a)
callable _ = do
    setupst <- get
    put $ setupst { counter = counter setupst + 1 }
    return $ Callable (counter setupst) []

(<.>) :: (Show a, Read a) => Callable (a -> b) -> a -> Callable b
Callable fun xs <.> x = Callable fun (show x : xs)

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

c_sg_wrapper :: String -> IO String
c_sg_wrapper str =
    withCString str $ \cInput ->
    allocaBytes 128 $ \outBuf ->
    alloca $ \lenPtr -> do
        c_sg cInput outBuf lenPtr
        len <- peek lenPtr
        peekCStringLen (outBuf, fromIntegral len)

sg :: (Show a, Read a) => Callable (Secure a) -> IO a
sg (Callable fun args) = do
    let msg = show (fun, args)
    r <- c_sg_wrapper msg

    return $ read r

nonSecure :: IO a -> Setup ()
nonSecure ns = do
    v <- liftIO ns
    return $ v `seq` ()

runSetup :: Setup () -> IO ()
runSetup (Setup s) = evalStateT s initialSetupState