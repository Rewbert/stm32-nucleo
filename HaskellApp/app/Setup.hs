{-# LANGUAGE MultiParamTypeClasses #-}
module Setup (Setup(..), SetupState(..), initialSetupState, BFILE,
              primHSerialize, primHDeserialize) where

import Control.Monad.State
import Foreign.Ptr

-- import Control.Monad.State.Class as STC
import Control.Monad.IO.Class as IOC

import HW
import Hardware

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

-- * Setup monad, for configurations that affect both applications

data SetupState = SetupState
  {
    nonSecureCallable :: [(Int, Ptr BFILE -> IO (Ptr BFILE))]
  , counter           :: Int
  }

initialSetupState :: SetupState
initialSetupState = SetupState [] 0

newtype Setup a = Setup (StateT SetupState IO a)

instance Functor Setup where
    fmap f (Setup stt) = Setup $ f <$> stt

instance Applicative Setup where
    pure x = Setup $ pure x

    Setup f <*> Setup x = Setup $ f <*> x

instance Monad Setup where
    Setup ma >>= k = Setup $ do
        x <- ma
        let Setup b = k x
        b

instance MonadState SetupState Setup where
    get = Setup $ get

    put s = Setup $ put s

-- TODO: This is potentially insecure... consider rewriting perhaps? This lets us run IO on the secure world during setup phase, but
-- that is perhaps OK. We still cannot run arbitrary IO actions on the secure side during runtime.
instance MonadIO Setup where
  liftIO io = Setup $ liftIO io