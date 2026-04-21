{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Effectless.Secure where

import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Marshal.Alloc (free, alloca)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peek, poke)
import Foreign.C.Types (CInt(..))
import Data.Word (Word8, Word32)
import Control.Monad.State
import Data.IORef
import Unsafe.Coerce

import Control.Monad.IO.Class

import Effectless.Setup

foreign import ccall "set_vtable_ptr" c_set_vtable_ptr :: Word32 -> IO ()
foreign import ccall "get_vtable_ptr" c_get_vtable_ptr :: IO Word32

foreign import ccall "openb_wr_mem"  c_openb_wr_mem :: IO (Ptr BFILE)
foreign import ccall "openb_rd_mem"  c_openb_rd_mem :: Ptr Word8 -> Int -> IO (Ptr BFILE)
foreign import ccall "get_mem"       c_get_mem      :: Ptr BFILE -> Ptr (Ptr Word8) -> Ptr Int -> IO ()
foreign import ccall "closeb_rd_mem" c_closeb       :: Ptr BFILE -> IO ()
foreign import ccall "getb"          c_getb         :: Ptr BFILE -> IO Int

foreign export ccall "c_handle_nsc_call" handle_nsc_call
    :: Ptr Word8 -> CInt -> Ptr Word8 -> Ptr CInt -> IO ()

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
    mkNSC :: a -> (Ptr BFILE -> IO (Ptr BFILE))

-- Base case: run the action, serialize result into a fresh write BFILE
instance NonSecureCallable (Secure a) where
    mkNSC (Secure ioa) = \_ -> do
        a   <- ioa
        wbf <- c_openb_wr_mem
        primHSerialize wbf a
        return wbf

-- Recursive case: deserialize one arg from the stream, recurse.
-- Consume the trailing '\n' that primHSerialize appends after each value,
-- so that the next checkversion call sees 'v' and not '\n'.
instance NonSecureCallable b => NonSecureCallable (a -> b) where
    mkNSC f = \inBf -> do
        x <- primHDeserialize inBf
        _ <- c_getb inBf   -- skip the '\n' appended by serialise
        mkNSC (f x) inBf

-- * API for the user to designate functions as NSC, and applying them

data Callable a = CallableDummy

callable :: (NonSecureCallable a) => a -> Setup (Callable a)
callable f = do
    let g inBf = mkNSC f inBf
    modify $ \st ->
        st { counter = counter st + 1
           , nonSecureCallable = (counter st, g) : nonSecureCallable st
           }
    return CallableDummy

(<.>) :: Callable (a -> b) -> a -> Callable b
(<.>) = error "the secure world cannot call the non secure world"

-- * Secure references, to enable mutable state in the secure world

type SRef a = IORef a

initialSRef :: a -> Setup (Secure (SRef a))
initialSRef a = do
    r <- liftIO $ newIORef a
    return $ return r

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

sg :: Callable (Secure a) -> IO a
sg _ = return $ error "the server should never execute this function"

nonSecure :: IO a -> Setup ()
nonSecure _ = return ()

-- | Make sure that the vTable persists
storeVTable :: [(Int, Ptr BFILE -> IO (Ptr BFILE))]
            -> IO (StablePtr [(Int, Ptr BFILE -> IO (Ptr BFILE))])
storeVTable vtbl = do
    sp <- newStablePtr vtbl
    let w = fromIntegral $ ptrToWordPtr $ castStablePtrToPtr sp
    c_set_vtable_ptr w
    return sp

-- | Reconstruct the vTable
getVTable :: IO [(Int, Ptr BFILE -> IO (Ptr BFILE))]
getVTable = do
    w <- c_get_vtable_ptr
    let sp = castPtrToStablePtr (wordPtrToPtr (fromIntegral w))
                :: StablePtr [(Int, Ptr BFILE -> IO (Ptr BFILE))]
    deRefStablePtr sp

{-

handle_nsc_call is the S-world counterpart to sg in NonSecure.hs. It is called
by the C gate in S/main.c, which unpacks the raw bytes from the NS BFILE and
passes them here.

The input buffer contains a serialised closure: [ funidx, arg1, arg2, ..., argn ]
We wrap it in a read-BFILE, deserialise the function index, look it up in the
vtable, and dispatch to the corresponding mkNSC handler. That handler reads the
remaining arguments from the same BFILE in sequence, executes the Secure action,
and writes the result to a fresh S-world write-BFILE.

The result is then extracted from that BFILE and copied into the pre-allocated
NS output buffer, which the NS world reads back after the gate returns.

We use a fresh S-world BFILE for the output rather than writing directly into an
NS-provided BFILE, because every BFILE operation in the MHS runtime (putb, get_mem,
etc.) calls CHECKBFILE, which asserts that the function pointer in the BFILE matches
the local copy of getb_mem. An NS-allocated BFILE holds the NS copy of that function,
which differs in address from the S copy, causing CHECKBFILE to fail.

An alternative would be to construct a fresh S-world BFILE backed by the NS output
buffer directly. This might work, but it is unclear how the S-world allocator would
handle growing that buffer if the result exceeds the initial allocation, since the
buffer lives in NS memory. I have not tried this.

NOTE: serialise appends a newline after each value. We must consume it with
c_getb before each subsequent IO.deserialize call, or the version check will fail.

-}
handle_nsc_call :: Ptr Word8 -> CInt -> Ptr Word8 -> Ptr CInt -> IO ()
handle_nsc_call inBuf inLen outBuf outLenPtr = do
    -- wrap the input bytes in a read-BFILE
    inBf   <- c_openb_rd_mem inBuf (fromIntegral inLen)

    -- deserialise the function index and look it up in the vtable
    funIdx <- primHDeserialize inBf :: IO Int
    _      <- c_getb inBf   -- skip trailing '\n' after funIdx serialization
    vTable <- getVTable

    -- deserialise the remaining arguments, and actually run the Secure action.
    -- Returns a fresh write-BFILE containing the serialised result
    outBf  <- lookupFun funIdx vTable inBf

    -- free the input BFILE
    c_closeb inBf

    -- copy the result into the NS-provided output buffer
    alloca $ \bufPtrPtr ->
      alloca $ \lenPtr -> do
        c_get_mem outBf bufPtrPtr lenPtr
        bufPtr <- peek bufPtrPtr
        bufLen <- peek lenPtr
        copyBytes outBuf bufPtr (fromIntegral bufLen)
        poke outLenPtr (fromIntegral bufLen)

        -- free resources related to the result
        free bufPtr
        c_closeb outBf

lookupFun :: Int -> [(Int, Ptr BFILE -> IO (Ptr BFILE))] -> (Ptr BFILE -> IO (Ptr BFILE))
lookupFun idx [] = error $ "cannot find fun with index " ++ show idx ++ "\r"
lookupFun idx ((idx', f):xs)
    | idx == idx' = f
    | otherwise   = lookupFun idx xs

runSetup :: Setup () -> IO ()
runSetup (Setup s) = do
    (a, (SetupState { nonSecureCallable = vTable })) <- runStateT s initialSetupState
    storeVTable vTable
    return ()

