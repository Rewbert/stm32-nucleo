module Setup (Setup, SetupState(..)) where

import Control.Monad.State

-- * Setup monad, for configurations that affect both applications

data SetupState = SetupState
  {
    nonSecureCallable :: [(Int, [String] -> IO String)]
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