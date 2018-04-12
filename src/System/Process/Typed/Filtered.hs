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

module System.Process.Typed.Filtered
    ( withFilteredProcess
    ) where

import System.Process.Typed (ProcessConfig, Process, withProcess)

import Control.Monad.IO.Class         (liftIO)
import Control.Concurrent.Async       (async, wait)
import Control.Concurrent.STM.TStream (newTStreamIO, withTStream, sinkTStream,
                                       sourceTStream)

import Data.Conduit


-- | Run a process with a filter over its output. The process is killed as soon
-- as the filter exits. The filter is run in a new thread, so the returned
-- `ConduitT` yields data as early as possible.
withFilteredProcess :: ProcessConfig stdin stdout stderr
                    -> (Process stdin stdout stderr -> ConduitT () a IO ())
                    -> ConduitT i a IO ()
withFilteredProcess pc f = do
    s <- liftIO newTStreamIO
    tId <- liftIO $ async $ withTStream s $ withProcess pc $ \p ->
        runConduit $ f p .| sinkTStream s
    sourceTStream s
    liftIO $ wait tId
