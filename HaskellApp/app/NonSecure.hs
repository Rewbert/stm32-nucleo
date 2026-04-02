{-# LANGUAGE InstanceSigs #-}
module NonSecure where

import Foreign.Ptr
import Foreign.Storable (peek)
import Foreign.Marshal.Alloc (free, alloca, allocaBytes)
import Foreign.C.Types (CInt(..))
import Data.Word (Word8)
import Control.Monad.State
import Control.Monad.IO.Class

import Setup

-- * BFILE foreign imports

{-

These are procedures that we must be able to invoke with BFILEs to pass serialised graphs around

BFILE appears to work as a stream, so a function in the secure world is modeled as a function of type
Ptr BFILE -> IO (Ptr BFILE). Rather than a list of String's, as was the case before, the nonsecure world
will write the arguments in sequence to the BFILE, and the secure world will read them in sequence.

When the nonsecure world packages the closure for the secure world, it will in sequence write all the required
information, like so: [ funidx, arg1, arg2, arg3, ..., argn ]

Result is a single value written once to the 'out' BFILE.

NOTE: One bug I encountered is that IO.serialize writes the graph to the BFILE, but then appends a newline.
This newline must be consumed when I run IO.deserialize, or else the version check will fail.

-}

foreign import ccall "sg.h sg"       c_sg           :: Ptr BFILE -> Ptr Word8 -> Ptr CInt -> IO ()
foreign import ccall "openb_wr_mem"  c_openb_wr_mem :: IO (Ptr BFILE)
foreign import ccall "openb_rd_mem"  c_openb_rd_mem :: Ptr Word8 -> Int -> IO (Ptr BFILE)
foreign import ccall "get_mem"       c_get_mem      :: Ptr BFILE -> Ptr (Ptr Word8) -> Ptr Int -> IO ()
foreign import ccall "closeb_rd_mem" c_closeb       :: Ptr BFILE -> IO ()

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
    mkNSC :: a -> (Ptr BFILE -> IO (Ptr BFILE))

instance NonSecureCallable (Secure a) where
    mkNSC _ = \_ -> error "NS world cannot invoke mkNSC"

instance NonSecureCallable b => NonSecureCallable (a -> b) where
    mkNSC _ = \_ -> error "NS world cannot invoke mkNSC"

-- * API for the user to designate functions as NSC, and applying them

-- A function is identified by a vtable index, and a list of IO actions that write an
-- argument to a given BFILE
data Callable a = Callable Int [Ptr BFILE -> IO ()]

callable :: (NonSecureCallable a) => a -> Setup (Callable a)
callable _ = do
    setupst <- get
    put $ setupst { counter = counter setupst + 1 }
    return $ Callable (counter setupst) []

(<.>) :: Callable (a -> b) -> a -> Callable b
Callable fun writes <.> x = Callable fun (writes ++ [\bf -> primHSerialize bf x]) -- perhaps I should not use ++ (inefficient), but our functions are of such low arity that maybe I don't care enough

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

-- * Invoking a non-secure-callable function in the secure world

-- Upper bound for the serialised result from the secure world.
-- It is quite large, but my new board has a butt-load of memory
--
-- I should do some evaluation of how large the serialised results/inputs
-- usually are... that would be interesting to see.
nscOutputBufSize :: Int
nscOutputBufSize = 4096

sg :: Callable (Secure a) -> IO a
sg (Callable fun writes) =
    -- Allocate space for the result
    alloca $ \outLenPtr ->
    allocaBytes nscOutputBufSize $ \outBuf -> do

        -- init BFILE
        wbf <- c_openb_wr_mem

        -- write the fun index
        primHSerialize wbf fun

        -- write the arguments
        mapM_ (\serialiseAction -> serialiseAction wbf) writes

        -- actually invoke the NSC function
        c_sg wbf outBuf outLenPtr

        -- free the allocated resources related to the closure
        alloca $ \bufPtrPtr ->
          alloca $ \lenPtr -> do
            c_get_mem wbf bufPtrPtr lenPtr
            peek bufPtrPtr >>= free
        c_closeb wbf

        -- construct the result BFILE from the memory buffer
        outLen <- peek outLenPtr
        rbf    <- c_openb_rd_mem outBuf (fromIntegral outLen)

        -- deserilise the result
        result <- primHDeserialize rbf

        -- free resources related to the result
        c_closeb rbf
        return result

nonSecure :: IO a -> Setup ()
nonSecure ns = do
    v <- liftIO ns
    return $ v `seq` ()

runSetup :: Setup () -> IO ()
runSetup (Setup s) = evalStateT s initialSetupState
