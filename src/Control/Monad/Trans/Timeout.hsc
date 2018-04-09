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
import           Control.Monad.IO.Class (MonadIO (liftIO))
##if MIN_VERSION_base(4, 4, 0)
import           Control.Monad.Zip (MonadZip (mzip, mzipWith, munzip))
##endif
import           Data.Functor.Identity (Identity (Identity))
import           Data.Int (Int32, Int64)
import           Data.Typeable (Typeable)
import           Data.Unique (Unique, newUnique)
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (peekByteOff)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadTrans
                     , lift
                     , MInvariant
                     , hoistiso
                     , MFunctor
                     , hoist
                     , MonadTransControl
                     , LayerResult
                     , LayerState
                     , suspend
                     , resume
                     , capture
                     , extract
                     , control
                     , MonadInner
                     , liftI
                     , MonadInnerControl
                     , controlI
                     )


-- timeout -------------------------------------------------------------------
import           Monad.Timeout (MonadTimeout (unpause, tickle, pause))


------------------------------------------------------------------------------
newtype TimeoutT m a = TimeoutT (TMVar Int64 -> m a)


------------------------------------------------------------------------------
instance MonadTrans TimeoutT where
    lift = TimeoutT . const


------------------------------------------------------------------------------
instance MInvariant TimeoutT where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MFunctor TimeoutT where
    hoist f (TimeoutT m) = TimeoutT $ f . m


------------------------------------------------------------------------------
instance MonadTransControl TimeoutT where
    suspend (TimeoutT m) t = liftM (\a -> (Identity a, t)) (m t)
    resume (Identity a, _) = TimeoutT $ \_ -> return a
    capture = TimeoutT return
    extract _ (Identity a) = Right a


------------------------------------------------------------------------------
type instance LayerResult TimeoutT = Identity
type instance LayerState TimeoutT = TMVar Int64


------------------------------------------------------------------------------
instance Monad m => Functor (TimeoutT m) where
    fmap = liftM


------------------------------------------------------------------------------
instance Monad m => Applicative (TimeoutT m) where
    pure = return
    (<*>) = ap


------------------------------------------------------------------------------
instance MonadPlus m => Alternative (TimeoutT m) where
    empty = mzero
    (<|>) = mplus


------------------------------------------------------------------------------
instance Monad m => Monad (TimeoutT m) where
    return = lift . return
    {-# INLINABLE return #-}
    TimeoutT m >>= f = TimeoutT $ \r -> m r >>= \a ->
        let TimeoutT m' = f a in m' r
    {-# INLINABLE (>>=) #-}
    fail = lift . fail
    {-# INLINABLE fail #-}


------------------------------------------------------------------------------
instance MonadPlus m => MonadPlus (TimeoutT m) where
    mzero = lift mzero
    mplus a b = control (\peel -> mplus (peel a) (peel b))


------------------------------------------------------------------------------
instance MonadFix m => MonadFix (TimeoutT m) where
    mfix f = control (\peel -> mfix (\a -> peel (resume a >>= f)))


##if MIN_VERSION_base(4, 4, 0)
------------------------------------------------------------------------------
instance MonadZip m => MonadZip (TimeoutT m) where
    mzipWith f = liftM2 f
    mzip = liftM2 (,)
    munzip m = (liftM fst m, liftM snd m)


##endif
------------------------------------------------------------------------------
instance MonadIO m => MonadIO (TimeoutT m) where
    liftIO = lift . liftIO


------------------------------------------------------------------------------
instance MonadInner IO m => MonadTimeout (TimeoutT m) where
    unpause = tickle

    tickle = TimeoutT $ \r -> liftI $ do
        time <- getTime
        atomically $ tryTakeTMVar r >>= putTMVar r . maybe time (max time)

    pause = TimeoutT $ \r -> liftI $ do
        _ <- atomically $ tryTakeTMVar r
        return ()


------------------------------------------------------------------------------
runTimeoutT :: MonadInnerControl IO m => Int64 -> TimeoutT m a -> m (Maybe a)
runTimeoutT n (TimeoutT m) = controlI $ \peel -> do
    pid <- myThreadId
    tickler <- getTime >>= atomically . newTMVar
    e <- liftM Message newUnique
    handleJust (guard . (== e)) (\_ -> peel $ return Nothing) (bracket
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
        (\_ -> peel . liftM Just $ m tickler))
  where
    threadDelay' t = do
        let t' = t - fromIntegral (maxBound :: Int)
        if t' > 0
            then threadDelay maxBound >> threadDelay' t'
            else threadDelay $ fromIntegral t


------------------------------------------------------------------------------
newtype Message = Message Unique deriving (Eq, Typeable)
instance Show Message where show _ =  "<<message>>"
instance Exception Message


#ifndef mingw32_os
#include <time.h>


------------------------------------------------------------------------------
getTime :: IO Int64
getTime = allocaBytes (#const sizeof (struct timespec)) $ \ptr -> do
    clock_gettime (#const CLOCK_MONOTONIC) ptr
    secs <- (#peek struct timespec, tv_sec) ptr
    nsecs <- (#peek struct timespec, tv_nsec) ptr
    return
        $ fromIntegral ((secs :: (#type time_t)) * 1000000)
        + fromIntegral ((nsecs :: (#type long)) `div` 1000)


------------------------------------------------------------------------------
foreign import ccall unsafe "clock_gettime"
    clock_gettime :: (#type clockid_t) -> Ptr a -> IO ()
#else
#define _WIN32_WINNT as 0x0600
#include <Windows.h>


------------------------------------------------------------------------------
getTime :: IO Int64
getTime = fmap (* 1000) getTickCount64


------------------------------------------------------------------------------
foreign import ccall unsafe "GetTickCount64"
    getTickCount64 :: IO Word64
#endif
