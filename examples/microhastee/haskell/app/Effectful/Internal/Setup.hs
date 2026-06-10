{-# LANGUAGE QualifiedDo #-}
module Effectful.Internal.Setup where

import Data.Word

import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr

import Control.Monad.State as ST
import Control.Monad.State.Class
import Control.Monad.IO.Class
import qualified Control.Monad.IxMonad as Ix

{-
I define these BFILE things here, because they are required by both the secure and nonsecure application.
They are re-exported for them both to use.
-}
data BFILE

-- These are primitives offered by the MHS RTS
primHSerialize   :: Ptr BFILE -> a -> IO ()
primHSerialize    = _primitive "IO.serialize"
primHDeserialize :: Ptr BFILE -> IO a
primHDeserialize  = _primitive "IO.deserialize"

data Setup nsEffects sEffects nsEffectsPost sEffectsPost a = Setup (StateT SetupState IO a)

liftSetupIO :: IO a -> Setup n1 n2 n3 n4 a
liftSetupIO ioa = Setup (liftIO ioa)

type ExtiCallback = (Int, Int, Int -> IO ())

data SetupState = SetupState
  {
    nonSecureCallable :: [(Int, Ptr BFILE -> IO (Ptr BFILE))]
  , counter           :: Int
  , extiCallbacks     :: [ExtiCallback]
  }

initialSetupState :: SetupState
initialSetupState = SetupState [] 0 []

get :: Setup i j i j SetupState
get = Setup $ ST.get

put :: SetupState -> Setup i j i j ()
put st = Setup $ ST.put st

modify :: (SetupState -> SetupState) -> Setup i j i j ()
modify f =
    Effectful.Internal.Setup.get `Ix.ibind` \st ->
    Effectful.Internal.Setup.put (f st)

foreign import ccall "exti_trampoline.h set_exti_callback_table_ptr" c_set_exti_callback_table_ptr :: Word32 -> IO ()
foreign import ccall "exti_trampoline.h get_exti_callback_table_ptr" c_get_exti_callback_table_ptr :: IO Word32

foreign export ccall "h_exti_dispatch" h_exti_dispatch :: CInt -> CInt -> IO ()

storeExtiCallbacks :: [ExtiCallback] -> IO (StablePtr [ExtiCallback])
storeExtiCallbacks callbacks = do
    sp <- newStablePtr callbacks
    let w = fromIntegral $ ptrToWordPtr $ castStablePtrToPtr sp
    c_set_exti_callback_table_ptr w
    return sp

getExtiCallbacks :: IO [ExtiCallback]
getExtiCallbacks = do
    w <- c_get_exti_callback_table_ptr
    if w == 0
      then return []
      else do
        let sp = castPtrToStablePtr (wordPtrToPtr (fromIntegral w))
                    :: StablePtr [ExtiCallback]
        deRefStablePtr sp

registerExtiCallback :: Int -> Int -> (Int -> IO ()) -> Setup ns s ns s ()
registerExtiCallback line edge cb = Setup $ do
    st <- ST.get
    let callbacks = (line, edge, cb) : extiCallbacks st
    ST.put st { extiCallbacks = callbacks }
    _ <- liftIO $ storeExtiCallbacks callbacks
    return ()

h_exti_dispatch :: CInt -> CInt -> IO ()
h_exti_dispatch line edge = do
    -- TODO: replace direct ISR dispatch with a pending event queue that
    -- ordinary Haskell code drains outside interrupt context.
    callbacks <- getExtiCallbacks
    dispatchExtiCallback (fromIntegral line) (fromIntegral edge) callbacks

dispatchExtiCallback :: Int -> Int -> [ExtiCallback] -> IO ()
dispatchExtiCallback _ _ [] = return ()
dispatchExtiCallback line edge ((line', edge', cb):callbacks)
    | line == line' && edgeMatches edge' edge = cb edge
    | otherwise = dispatchExtiCallback line edge callbacks

edgeMatches :: Int -> Int -> Bool
edgeMatches wanted actual = wanted == 2 || wanted == actual

instance Ix.IxMonad Setup where

    ireturn = Setup . return

    ibind (Setup m) k = Setup $ do
        a <- m
        let Setup iob = k a
        iob
    
    ilift ioa = Setup $ liftIO ioa
