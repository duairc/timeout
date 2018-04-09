{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#include "overlap.h"

module Monad.Timeout
    ( MonadTimeout (tickle, pause, unpause)
    )
where

-- base ----------------------------------------------------------------------
import           Data.Functor.Product (Product (Pair))


-- mmorph --------------------------------------------------------------------
import           Control.Monad.Trans.Compose (ComposeT (ComposeT))


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (MonadTrans, lift)


------------------------------------------------------------------------------
class Monad m => MonadTimeout m where
    tickle :: m ()
    pause :: m ()
    unpause :: m ()


------------------------------------------------------------------------------
instance MonadTimeout IO where
    tickle = return ()
    pause = return ()
    unpause = return ()


------------------------------------------------------------------------------
instance (MonadTimeout f, MonadTimeout g) => MonadTimeout (Product f g) where
    tickle = Pair tickle tickle
    pause = Pair pause pause
    unpause = Pair unpause unpause


------------------------------------------------------------------------------
instance MonadTimeout (f (g m)) => MonadTimeout (ComposeT f g m) where
    tickle = ComposeT tickle
    pause = ComposeT pause
    unpause = ComposeT unpause


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (MonadTrans t, MonadTimeout m, Monad (t m)) =>
    MonadTimeout (t m)
  where
    tickle = lift tickle
    pause = lift pause
    unpause = lift unpause
