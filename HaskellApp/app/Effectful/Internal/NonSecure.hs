{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QualifiedDo #-}
module Effectful.Internal.NonSecure where

import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.Types
import qualified Control.Monad.IxMonad as Ix
import qualified Control.Monad.State as ST

import Effectful.TypeLevel.List
import Effectful.Internal.Setup

foreign import ccall "sg.h sg"       c_sg           :: Ptr BFILE -> Ptr Word8 -> Ptr CInt -> IO ()
foreign import ccall "openb_wr_mem"  c_openb_wr_mem :: IO (Ptr BFILE)
foreign import ccall "openb_rd_mem"  c_openb_rd_mem :: Ptr Word8 -> Int -> IO (Ptr BFILE)
foreign import ccall "get_mem"       c_get_mem      :: Ptr BFILE -> Ptr (Ptr Word8) -> Ptr Int -> IO ()
foreign import ccall "closeb_rd_mem" c_closeb       :: Ptr BFILE -> IO ()

data Secure effects a = SecureDummy
data Nonsecure effects a = Nonsecure (IO a)

nonsecureLiftIO :: IO a -> Nonsecure effects a
nonsecureLiftIO ioa = Nonsecure ioa

secureLiftIO :: IO a -> Secure effects a
secureLiftIO ioa = SecureDummy

instance Functor (Secure effects) where
    fmap f SecureDummy = SecureDummy

instance Applicative (Secure effects) where
    pure _ = SecureDummy
    SecureDummy <*> SecureDummy = SecureDummy

instance Monad (Secure effects) where
    SecureDummy >>= _ = SecureDummy

instance Functor (Nonsecure effects) where
    fmap f (Nonsecure ioa) = Nonsecure $ fmap f ioa

instance Applicative (Nonsecure effects) where
    pure x = Nonsecure $ pure x
    (Nonsecure f) <*> (Nonsecure a) = Nonsecure $ f <*> a

instance Monad (Nonsecure effects) where
    Nonsecure ioa >>= k = Nonsecure $ do
        a <- ioa
        let Nonsecure iob = k a
        iob

-- * NSC API

class NonSecureCallable effects a | a -> effects where
    mkNSC :: a -> (Ptr BFILE -> IO (Ptr BFILE))

instance NonSecureCallable effects (Secure effects a) where
    mkNSC _ = \_ -> error "NS world cannot invoke mkNSC"

instance NonSecureCallable effects b => NonSecureCallable effects (a -> b) where
    mkNSC _ = \_ -> error "NS world cannot invoke mkNSC"

-- A function is identified by a vtable index, and a list of IO actions that write an
-- argument to a given BFILE
data Callable a = Callable Int [Ptr BFILE -> IO ()]

callable :: (NonSecureCallable s a) => a -> Setup ns s ns s (Callable a)
callable _ = Ix.do
    setupst <- get
    put $ setupst { counter = counter setupst + 1 }
    Ix.return $ Callable (counter setupst) []

(<.>) :: Callable (a -> b) -> a -> Callable b
Callable fun writes <.> x = Callable fun (writes ++ [\bf -> primHSerialize bf x]) -- perhaps I should not use ++ (inefficient), but our functions are of such low arity that maybe I don't care enough

-- Upper bound for the serialised result from the secure world.
-- It is quite large, but my new board has a butt-load of memory
--
-- I should do some evaluation of how large the serialised results/inputs
-- usually are... that would be interesting to see.
nscOutputBufSize :: Int
nscOutputBufSize = 4096

sg :: Callable (Secure seffects a) -> Nonsecure nseffects a
sg (Callable fun writes) = nonsecureLiftIO $
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

-- * Secure state

data SRef a = SRefDummy

initialSRef :: forall a ns s effects . a -> Setup ns s ns s (Secure effects (SRef a))
initialSRef a = Ix.return $ SecureDummy

newSRef :: a -> Secure effects (SRef a)
newSRef _ = SecureDummy

readSRef :: SRef a -> Secure effects a
readSRef _ = SecureDummy

writeSRef :: SRef a -> a -> Secure effects ()
writeSRef _ _ = SecureDummy

modifySRef :: SRef a -> (a -> a) -> Secure effects ()
modifySRef _ _ = SecureDummy

nonsecure :: Nonsecure ns () -> Setup ins is ns s ()
nonsecure (Nonsecure ioa) = liftSetupIO ioa

-- * Entry point

runSetup :: Setup Nil Nil ns s () -> IO ()
runSetup (Setup s) = do
    () <- ST.evalStateT s initialSetupState
    return ()