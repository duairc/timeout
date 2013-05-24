{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Timeout
    ( TimeoutT
    , runTimeoutT
    )
where

#include <sys/time.h>

-- base ----------------------------------------------------------------------
import           Control.Applicative
                     ( Applicative (pure, (<*>))
                     , Alternative (empty, (<|>))
                     )
import           Control.Concurrent
                     ( forkIO
                     , killThread
                     , myThreadId
                     , threadDelay
                     , throwTo
                     )
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TMVar
                     ( TMVar
                     , newTMVar
                     , putTMVar
                     , readTMVar
                     , tryTakeTMVar
                     )
import           Control.Exception (Exception, bracket, handleJust)
import           Control.Monad
                     ( MonadPlus (mzero, mplus)
                     , ap
                     , guard
                     , liftM
##if MIN_VERSION_base(4, 4, 0)
                     , liftM2
##endif
                     )
import           Control.Monad.Fix (MonadFix (mfix))
##if MIN_VERSION_base(4, 4, 0)
import           Control.Monad.Zip (MonadZip (mzip, mzipWith, munzip))
##endif
import           Data.Int (Int64)
import           Data.Typeable (Typeable)
import           Data.Unique (newUnique)
import           Foreign.C.Types
                     (  CInt
##if MIN_VERSION_base(4, 5, 0)
                         (CInt)
##endif
                     )
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Ptr (Ptr, nullPtr, plusPtr)
import           Foreign.Storable (peek)


-- transformers --------------------------------------------------------------
import qualified Control.Monad.Trans.Class as T (MonadTrans (lift))
import           Control.Monad.IO.Class (MonadIO (liftIO))


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer
                     ( MonadLayer (type Inner, layer, layerInvmap)
                     , MonadLayerFunctor (layerMap)
                     , MonadLayerControl
                         ( type LayerState
                         , restore
                         , layerControl
                         )
#if __GLASGOW_HASKELL__ >= 702
                     , MonadTrans (type Outer, transInvmap)
                     , MonadTransFunctor (transMap)
                     , MonadTransControl (transControl)
#endif
                     , MonadLift (lift)
                     , MonadLiftControl
                     , control
                     , controlLayer
                     )


-- timeout -------------------------------------------------------------------
import           Control.Monad.Interface.Timeout
                     ( MonadTimeout (resume, tickle, pause)
                     )


------------------------------------------------------------------------------
newtype TimeoutT m a = TimeoutT (TMVar Int64 -> m a)


------------------------------------------------------------------------------
instance T.MonadTrans TimeoutT where
    lift = layer
    {-# INLINE lift #-}


------------------------------------------------------------------------------
instance Monad m => Functor (TimeoutT m) where
    fmap = liftM
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
instance Monad m => Applicative (TimeoutT m) where
    pure = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}


------------------------------------------------------------------------------
instance MonadPlus m => Alternative (TimeoutT m) where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}


------------------------------------------------------------------------------
instance Monad m => Monad (TimeoutT m) where
    return = layer . return
    {-# INLINE return #-}
    TimeoutT m >>= f = TimeoutT $ \r -> m r >>= \a ->
        let TimeoutT m' = f a in m' r
    {-# INLINE (>>=) #-}
    fail = layer . fail
    {-# INLINE fail #-}


------------------------------------------------------------------------------
instance MonadPlus m => MonadPlus (TimeoutT m) where
    mzero = layer mzero
    {-# INLINE mzero #-}
    mplus a b = controlLayer (\run -> mplus (run a) (run b))
    {-# INLINE mplus #-}


------------------------------------------------------------------------------
instance MonadFix m => MonadFix (TimeoutT m) where
    mfix f = controlLayer (\run -> mfix (\a -> run (restore a >>= f)))
    {-# INLINE mfix #-}


##if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
instance MonadZip m => MonadZip (TimeoutT m) where
    mzipWith f = liftM2 f
    {-# INLINE mzipWith #-}
    mzip = liftM2 (,)
    {-# INLINE mzip #-}
    munzip m = (liftM fst m, liftM snd m)
    {-# INLINE munzip #-}
##endif


------------------------------------------------------------------------------
instance MonadIO m => MonadIO (TimeoutT m) where
    liftIO = layer . liftIO
    {-# INLINE liftIO #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayer (TimeoutT m) where
    type Inner (TimeoutT m) = m
    layer = TimeoutT . const
    {-# INLINE layer #-}
    layerInvmap (f, _) = layerMap f
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerFunctor (TimeoutT m) where
    layerMap f (TimeoutT m) = TimeoutT $ f . m
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerControl (TimeoutT m) where
    newtype LayerState (TimeoutT m) a = L {unL :: a}
    restore = TimeoutT . const . return . unL
    {-# INLINE restore #-}
    layerControl f = TimeoutT $ \r -> f $ \(TimeoutT t) -> liftM L $ t r
    {-# INLINE layerControl #-}


#if __GLASGOW_HASKELL__ >= 702
------------------------------------------------------------------------------
instance Monad m => MonadTrans (TimeoutT m) where
    type Outer (TimeoutT m) = TimeoutT
    transInvmap (f, _) = transMap f
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransFunctor (TimeoutT m) where
    transMap f (TimeoutT m) = TimeoutT $ f . m
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransControl (TimeoutT m) where
    transControl f = TimeoutT $ \r -> f $ \(TimeoutT t) -> liftM L $ t r
    {-# INLINE transControl #-}
#endif


------------------------------------------------------------------------------
instance MonadLift IO m => MonadTimeout (TimeoutT m) where
    resume = tickle

    tickle = TimeoutT $ \r -> lift $ do
        time <- getTime
        atomically $ tryTakeTMVar r >>= putTMVar r . maybe time (max time)

    pause = TimeoutT $ \r -> lift $ do
        _ <- atomically $ tryTakeTMVar r
        return ()


------------------------------------------------------------------------------
runTimeoutT :: MonadLiftControl IO m => Int64 -> TimeoutT m a -> m (Maybe a)
runTimeoutT n (TimeoutT m) = control $ \run -> do
    pid <- myThreadId
    tickler <- getTime >>= atomically . newTMVar
    e <- liftM Message newUnique
    handleJust (guard . (== e)) (\_ -> run $ return Nothing) (bracket
        (forkIO $ do
            let go = do
                kill <- liftM (+n) . atomically $ readTMVar tickler
                now <- getTime
                let delta = kill - now
                if delta > 0
                   then threadDelay' delta >> go
                   else throwTo pid e
            go)
        killThread
        (\_ -> run . liftM Just $ m tickler))
  where
    threadDelay' t = do
        let t' = t - fromIntegral (maxBound :: Int)
        if t' > 0
            then threadDelay maxBound >> threadDelay' t'
            else threadDelay $ fromIntegral t


------------------------------------------------------------------------------
newtype Message a = Message a deriving (Eq, Typeable)
instance Show (Message a) where show _ =  "<<message>>"
instance Typeable a => Exception (Message a)


------------------------------------------------------------------------------
getTime :: IO Int64
getTime = allocaBytes (#const sizeof (struct timeval)) $ \ptr -> do
    _ <- gettimeofday ptr nullPtr
    secs <- peek ptr
    usecs <- peek $ ptr `plusPtr` (#const sizeof (time_t))
    return
        $ fromIntegral ((secs :: (#type time_t)) * 1000000)
        + fromIntegral (usecs :: (#type suseconds_t))


------------------------------------------------------------------------------
foreign import ccall unsafe gettimeofday :: Ptr a -> Ptr a -> IO CInt
