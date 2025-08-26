{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.State (StateT(..), evalStateT, execStateT, module Control.Monad.State.Class) where

import Control.Monad.State.Class
import Control.Monad.IO.Class

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = do
    p <- runStateT m s
    return $ fst p

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = do
    p <- runStateT m s
    return $ snd p

-- state :: (Monad m)
--       => (s -> (a, s))  -- ^pure state transformer
--       -> StateT s m a   -- ^equivalent state-passing computation
-- state f = StateT (return . f)

instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (a, s)
    StateT mf <*> StateT mx = StateT $ \ s -> do
        ~(f, s') <- mf s
        ~(x, s'') <- mx s'
        return (f x, s'')
    m *> k = m >>= \_ -> k

instance (Monad m) => Monad (StateT s m) where
    m >>= k  = StateT $ \ s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO
      where
        lift m = StateT $ \s -> do
                   a <- m
                   return (a, s)

instance Monad m => MonadState s (StateT s m) where
    state f = StateT (return . f)

-- get :: (Monad m) => StateT s m s
-- get = state $ \ s -> (s, s)

-- -- | @'put' s@ sets the state within the monad to @s@.
-- put :: (Monad m) => s -> StateT s m ()
-- put s = state $ \ _ -> ((), s)

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = state $ \ s -> ((), f s)