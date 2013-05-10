{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Interface.Timeout
    ( MonadTimeout (tickle, pause, resume)
    )
where

-- layers --------------------------------------------------------------------
import           Control.Monad.Layer (MonadLayer (type Inner, layer))


------------------------------------------------------------------------------
class Monad m => MonadTimeout m where
    tickle :: m ()
    pause :: m ()
    resume :: m ()


------------------------------------------------------------------------------
instance MonadTimeout IO where
    tickle = return ()
    {-# INLINE tickle #-}
    pause = return ()
    {-# INLINE pause #-}
    resume = return ()
    {-# INLINE resume #-}


------------------------------------------------------------------------------
instance (MonadLayer m, MonadTimeout (Inner m)) => MonadTimeout m where
    tickle = layer tickle
    {-# INLINE tickle #-}
    pause = layer pause
    {-# INLINE pause #-}
    resume = layer resume
    {-# INLINE resume #-}