{-# LANGUAGE QualifiedDo #-}
module Effectful.Internal.Setup where

import Foreign.Ptr

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

data SetupState = SetupState
  {
    nonSecureCallable :: [(Int, Ptr BFILE -> IO (Ptr BFILE))]
  , counter           :: Int
  }

initialSetupState :: SetupState
initialSetupState = SetupState [] 0

get :: Setup i j i j SetupState
get = Setup $ ST.get

put :: SetupState -> Setup i j i j ()
put st = Setup $ ST.put st

modify :: (SetupState -> SetupState) -> Setup i j i j ()
modify f =
    Effectful.Internal.Setup.get `Ix.ibind` \st ->
    Effectful.Internal.Setup.put (f st)

instance Ix.IxMonad Setup where

    ireturn = Setup . return

    ibind (Setup m) k = Setup $ do
        a <- m
        let Setup iob = k a
        iob
    
    ilift ioa = Setup $ liftIO ioa