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
--
-- The functions in the module assume filenames according to
-- `Web.YouTube.DateFormat.format`.
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Web.YouTube.DateFormat.Latest
    ( getLatestIds
    , latestGot
    , downloadLatest
    ) where

import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Catch      (throwM)
import Control.Monad.Catch.Pure (Catch)
import Control.Exception        (Exception)
import Control.Foldl.Extra      (collectLargest)
import Control.Foldl            (generalize, premap, handles, folded)

import Data.Function (fix)
import Data.Conduit
import Data.Set      (Set, singleton, member)

import System.Directory.Foldl (foldDirectoryContents)

import Web.YouTube            (youtubeDl')
import Web.YouTube.Playlist   (getIds)
import Web.YouTube.DateFormat (parse, uploadDate, videoId, format)


data PlaylistGap = PlaylistGap
  deriving Show

instance Exception PlaylistGap

-- | Get video ids of the latest videos in a playlist that haven't already been
-- gotten.
getLatestIds :: String     -- ^ Playlist
             -> Set String -- ^ Video ids that have already been gotten
             -> ConduitT i String IO ()
getLatestIds x got = getIds [x] (untilGot got)

-- | Get video ids of the latest videos in a directory.
latestGot :: FilePath -> IO (Set String)
latestGot fp = foldDirectoryContents fp $
    generalize $
    premap parse $
    handles folded $
    collectLargest uploadDate (singleton . videoId)

-- | Download the latest videos in a playlist that haven't already been gotten.
downloadLatest :: FilePath -- ^ Directory to store videos in
               -> String   -- ^ Playlist
               -> [String] -- ^ Extra options to pass to youtube-dl
               -> IO ()
downloadLatest fp playlist options = runConduit
    $ (getLatestIds playlist =<< liftIO (latestGot fp))
   .| awaitForever (\x -> liftIO $ youtubeDl' fp $ options ++ ["-o", format,
                                                   "--", x])

untilGot :: Set String -> ConduitT String String Catch ()
untilGot got = fix $ \r -> await >>= \case
    Nothing -> throwM PlaylistGap
    Just  x -> if member x got
        then pure ()
        else yield x *> r
