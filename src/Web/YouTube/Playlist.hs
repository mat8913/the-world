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

{-# LANGUAGE ExistentialQuantification #-}

module Web.YouTube.Playlist
    ( getIds
    ) where

import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Catch.Pure (Catch, runCatch)
import Control.Exception        (throwIO)

import Data.Conduit
import Data.Conduit.Combinators (yieldMany)

import System.IO                     (hGetContents)
import System.Process.Typed.Filtered (withFilteredProcess)
import System.Process.Typed          (proc, getStdout, setStdin, setStdout,
                                      setStderr, createPipe, closed)

import Web.YouTube (ytdlCommand)


playlistLimit :: Int
playlistLimit = 200

-- | Get video ids from one or more playlists.
getIds :: [String] -> ConduitT String a Catch () -> ConduitT i a IO ()
getIds xs f = withFilteredProcess pc $ \p
    -> (fmap lines (liftIO $ hGetContents $ getStdout p) >>= yieldMany)
    .| transPipe runCatchIO f
  where
    args = "--get-id"
         : ("--playlist-end=" ++ show playlistLimit)
         : xs
    pc = setStdin closed $
         setStdout createPipe $
         setStderr closed $
         proc ytdlCommand args

runCatchIO :: Catch a -> IO a
runCatchIO = either throwIO pure . runCatch
