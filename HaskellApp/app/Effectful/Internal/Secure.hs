{-# LANGUAGE QualifiedDo #-}
module Effectful.Internal.Secure where

import Data.Word
import Data.IORef

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.C.Types

import Effectful.Internal.Setup
import qualified Control.Monad.IxMonad as Ix
import Control.Monad.IO.Class
import qualified Control.Monad.State as ST

foreign import ccall "set_vtable_ptr" c_set_vtable_ptr :: Word32 -> IO ()
foreign import ccall "get_vtable_ptr" c_get_vtable_ptr :: IO Word32

foreign import ccall "openb_wr_mem"  c_openb_wr_mem :: IO (Ptr BFILE)
foreign import ccall "openb_rd_mem"  c_openb_rd_mem :: Ptr Word8 -> Int -> IO (Ptr BFILE)
foreign import ccall "get_mem"       c_get_mem      :: Ptr BFILE -> Ptr (Ptr Word8) -> Ptr Int -> IO ()
foreign import ccall "closeb_rd_mem" c_closeb       :: Ptr BFILE -> IO ()
foreign import ccall "getb"          c_getb         :: Ptr BFILE -> IO Int

foreign export ccall "c_handle_nsc_call" handle_nsc_call
    :: Ptr Word8 -> CInt -> Ptr Word8 -> Ptr CInt -> IO ()

-- * Secure

data Secure effects a = Secure (IO a)

secureLiftIO :: IO a -> Secure effects a
secureLiftIO ioa = Secure ioa

instance Functor (Secure effects) where
    fmap f (Secure ioa) = Secure $ fmap f ioa

instance Applicative (Secure effects) where
    pure x = Secure $ pure x

    Secure fa <*> Secure x = Secure $ fa <*> x

instance Monad (Secure effects) where
    Secure ioa >>= k = Secure $ do
        a <- ioa
        let Secure iob = k a
        iob

-- * NonSecure

data Nonsecure effects a = NonSecure

nonsecureLiftIO :: IO a -> Nonsecure effects a
nonsecureLiftIO ioa = NonSecure

instance Functor (Nonsecure effects) where
    fmap f NonSecure = NonSecure
instance Applicative (Nonsecure effects) where
    pure _ = NonSecure

    NonSecure <*> NonSecure = NonSecure

instance Monad (Nonsecure effects) where
    NonSecure >>= _ = NonSecure

-- * NSC API

class NonSecureCallable a where
    mkNSC :: a -> (Ptr BFILE -> IO (Ptr BFILE))

instance NonSecureCallable (Secure effects a) where
    mkNSC (Secure ioa) = \_ -> do
        a   <- ioa
        wbf <- c_openb_wr_mem
        primHSerialize wbf a
        return wbf

instance NonSecureCallable b => NonSecureCallable (a -> b) where
    mkNSC f = \inBf -> do
        x <- primHDeserialize inBf
        _ <- c_getb inBf   -- skip the '\n' appended by serialise
        mkNSC (f x) inBf

data Callable a = CallableDummy

callable :: (NonSecureCallable a) => a -> Setup ns s ns s (Callable a)
callable f = Ix.do
    let g inBf = mkNSC f inBf
    modify $ \st ->
        st { counter = counter st + 1
           , nonSecureCallable = (counter st, g) : nonSecureCallable st
           }
    Ix.return CallableDummy

(<.>) :: Callable (a -> b) -> a -> Callable b
(<.>) = error "the secure world cannot call the non secure world"

sg :: Callable (Secure seffects a) -> Nonsecure nseffects a
sg _ = return $ error "the server should never execute this function"

-- * Secure state

type SRef a = IORef a

initialSRef :: forall a ns s effects . a -> Setup ns s ns s (Secure effects (SRef a))
initialSRef a = Ix.do
    r <- Ix.ilift $ newIORef a
    Ix.return $ return r

readSRef :: SRef a -> Secure effects a
readSRef ref = Secure $ readIORef ref

writeSRef :: SRef a -> a -> Secure effects ()
writeSRef ref a = Secure $ writeIORef ref a

modifySRef :: SRef a -> (a -> a) -> Secure effects ()
modifySRef ref f = do
    v <- readSRef ref
    writeSRef ref (f v)

-- * Internal communications functions

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

-- * Entry point

nonsecure :: Nonsecure ns () -> Setup ins is ns s ()
nonsecure NonSecure = liftSetupIO (return ())

runSetup :: Setup () () ns s () -> IO ()
runSetup (Setup s) = do
    (a, (SetupState { nonSecureCallable = vTable })) <- ST.runStateT s initialSetupState
    storeVTable vTable
    return ()