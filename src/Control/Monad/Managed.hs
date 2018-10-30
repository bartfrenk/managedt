{-# LANGUAGE RankNTypes #-}

module Control.Monad.Managed where

import           Control.Monad.Logger
import           Control.Monad.Trans  (MonadIO, MonadTrans (..), liftIO)
import           Control.Monad.Trans  as Trans

data ManagedT m a = ManagedT { (>>-) :: forall r. (a -> m r) -> m r }

instance Functor (ManagedT m) where
  fmap f mx = ManagedT $ \return_ ->
    mx >>- \x -> return_ (f x)

instance Applicative (ManagedT m) where
  pure a = ManagedT $ \return_ -> return_ a

  mf <*> mx = ManagedT $ \return_ ->
    mf >>- \f ->
    mx >>- \x ->
    return_ (f x)

instance Monad (ManagedT m) where
  return r = ManagedT $ \return_ -> return_ r

  ma >>= f = ManagedT $ \return_ ->
    ma >>- \a ->
    f a >>- \b ->
    return_ b

instance MonadIO m => MonadIO (ManagedT m) where
  liftIO m = ManagedT $ \return_ -> do
    a <- liftIO m
    return_ a

instance MonadTrans ManagedT where
  lift m = ManagedT $ \return_ -> do
    a <- m
    return_ a

instance MonadLogger m => MonadLogger (ManagedT m) where
  monadLoggerLog a b c d = Trans.lift $ monadLoggerLog a b c d

runManagedT :: Monad m => ManagedT m a -> m a
runManagedT m = m >>- return

managedT :: (forall r. (a -> m r) -> m r) -> ManagedT m a
managedT = ManagedT
