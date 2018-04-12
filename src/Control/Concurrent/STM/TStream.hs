-- Copyright (C) 2018  Matthew Harm Bekkema
--
-- This file is part of the-world.
--
-- the-world is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- the-world is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

-----------------------------------------------------------------------------
-- |
-- Copyright   : Matthew Harm Bekkema 2018
-- License     : GPL-3
-- Maintainer  : mbekkema97@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TStream
    ( TStream
    , TStreamClosed (..)
    , newTStream
    , newTStreamIO
    , writeTStream
    , readTStream
    , closeTStream
    , sinkTStream
    , sourceTStream
    , withTStream
    ) where

import Control.Exception             (Exception, finally)
import Control.Monad.IO.Class        (MonadIO, liftIO)
import Control.Monad.STM             (STM, retry, orElse, atomically, throwSTM)
import Control.Concurrent.STM.TVar   (TVar, newTVar, newTVarIO, readTVar,
                                      writeTVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, newTQueueIO,
                                      readTQueue, writeTQueue)

import Data.Conduit  (ConduitT, awaitForever, yield)
import Data.Function (fix)
import Data.Bool     (bool)


-- | Represents a FIFO queue which can eventually be closed. A closed `TStream`
-- can no longer be written to.
data TStream a = TStream {-# UNPACK #-} !(TQueue a)
                         {-# UNPACK #-} !(TVar Bool) -- ^ Is stream done

-- | Thrown when attempting to write to a closed `TStream`.
data TStreamClosed = TStreamClosed
  deriving Show

instance Exception TStreamClosed

-- | Create a new instance of `TStream`.
newTStream :: STM (TStream a)
newTStream = TStream <$> newTQueue <*> newTVar False

-- | @IO@ version of `newTStream`.
newTStreamIO :: IO (TStream a)
newTStreamIO = TStream <$> newTQueueIO <*> newTVarIO False

-- | Write a value to a `TStream`. Throws `TStreamClosed` if the `TStream` is
-- closed.
writeTStream :: TStream a -> a -> STM ()
writeTStream (TStream xs d) x = do
    d' <- readTVar d
    if d'
        then throwSTM TStreamClosed
        else writeTQueue xs x

-- | Read the next value from a `TStream`. Returns `Nothing` if the `TStream` is
-- both empty and closed.
readTStream :: TStream a -> STM (Maybe a)
readTStream (TStream xs d) = orElse
    (Just <$> readTQueue xs)
    (readTVar d >>= bool retry (pure Nothing))

-- | Close a `TStream`.
closeTStream :: TStream a -> STM ()
closeTStream (TStream _ d) = writeTVar d True

-- | Sink all incoming data into a `TStream`. Does not close the `TStream` when
-- done.
sinkTStream :: MonadIO m => TStream i -> ConduitT i o m ()
sinkTStream xs = awaitForever $ liftIO . atomically . writeTStream xs

-- | Yield all data from a `TStream`.
sourceTStream :: MonadIO m => TStream o -> ConduitT i o m ()
sourceTStream xs = fix $ \r -> do
    mx <- liftIO $ atomically $ readTStream xs
    case mx of
        Nothing -> pure ()
        Just  x -> yield x *> r

-- | Run an action and ensure the `TStream` is closed when the action completes.
withTStream :: TStream a -> IO x -> IO x
withTStream s x = x `finally` atomically (closeTStream s)
