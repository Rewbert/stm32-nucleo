{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Secure where

import Foreign.Storable
import Foreign.Ptr
import Foreign.StablePtr
import Control.Monad.State
import Data.IORef
import Unsafe.Coerce
import Data.Word
import Foreign.C.String


import Setup
import Foreign.C.String

foreign import ccall "set_vtable_ptr" c_set_vtable_ptr :: Word32 -> IO ()
foreign import ccall "get_vtable_ptr" c_get_vtable_ptr :: IO Word32

foreign export ccall "c_handle_nsc_call" handle_nsc_call :: CString -> IO CString

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

unsafeLiftIO :: IO a -> Secure a
unsafeLiftIO = Secure

-- * Embedding NonSecureCallable functions

class NonSecureCallable a where
    mkNSC :: a -> ([String] -> Secure String)

instance (Show a, Read a) => NonSecureCallable (Secure a) where
    mkNSC v = \_ -> fmap show v

instance (Show a, Read a, NonSecureCallable b) => NonSecureCallable (a -> b) where
    mkNSC f = \inp -> case inp of
                        []     -> error "cannot have empty list here"
                        (x:xs) -> mkNSC (f $ read x) xs

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

-- | Make sure that the vTable persists
storeVTable :: [(Int, [String] -> IO String)] -> IO (StablePtr ([(Int, [String] -> IO String)]))
storeVTable vtbl = do
    sp <- newStablePtr vtbl
    let w = fromIntegral $ ptrToWordPtr $ castStablePtrToPtr sp
    c_set_vtable_ptr w
    return sp

-- | Reconstruct the vTable
getVTable :: IO ([(Int, [String] -> IO String)])
getVTable = do
    w <- c_get_vtable_ptr
    let sp = castPtrToStablePtr (wordPtrToPtr (fromIntegral w)) :: StablePtr ([(Int, [String] -> IO String)])
    deRefStablePtr sp

handle_nsc_call :: CString -> IO CString
handle_nsc_call cstr = do
    str <- peekCAString cstr
    let (funIdx, args) = read str :: (Int, [String])

    vTable <- getVTable
    let fun = lookupFun funIdx vTable

    res <- fun args
    newCAString res

lookupFun :: Int -> [(Int, [String] -> IO String)] -> ([String] -> IO String)
lookupFun idx [] = error $ "cannot find fun with index " ++ show idx ++ "\r"
lookupFun idx ((idx', f):xs)
    | idx == idx' = f
    | otherwise   = lookupFun idx xs

runSetup :: Setup () -> IO ()
runSetup (Setup s) = do
    (a, (SetupState { nonSecureCallable = vTable })) <- runStateT s initialSetupState
    -- now, here is where we wish to return and let the normal world execute. In normal Haskell,
    -- we would go to sleep here until some event wakes us up. On the board we cannot do this, so we
    -- instead need to make sure that the vTable we've just created isn't GC'd, and then return to
    -- nonsecure world.
    --
    -- When a NSC function is called we need to make sure that we can query the vTable for the function.

    -- bindvTable vTable

    putStrLn $ "produced a vTable with length: " ++ show (length vTable) ++ "\r"

    return ()
