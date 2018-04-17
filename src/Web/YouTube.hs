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

module Web.YouTube
    ( Middleware (..)
    , ytdlCommand
    , youtubeDl
    , youtubeDl'
    ) where

import Prelude hiding (mapM_)

import Control.Foldl  (mapM_)
import Data.Semigroup (Semigroup ((<>)))

import System.FilePath        ((</>))
import System.IO.Temp         (withTempDirectory)
import System.Directory       (renamePath)
import System.Directory.Foldl (foldDirectoryContents)
import System.Process.Typed   (runProcess_, setWorkingDir, proc)


-- | Middleware to be run after downloading videos.
--
-- `youtubeDl` will save videos into a temporary directory, then run the
-- `Middleware` with the temporary directory as the `FilePath` argument before
-- finally moving the contents of the temporary directory into the destination
-- directory.
newtype Middleware = Middleware (FilePath -> IO ())

instance Semigroup Middleware where
    Middleware f <> Middleware g = Middleware $ \x -> f x *> g x

instance Monoid Middleware where
    mempty = Middleware $ const $ pure ()
    mappend = (<>)

-- | The youtube-dl command.
ytdlCommand :: FilePath
ytdlCommand = "youtube-dl"

-- | Run youtube-dl.
youtubeDl :: FilePath   -- ^ Destination directory
          -> Middleware
          -> [String]   -- ^ Arguments
          -> IO ()
youtubeDl fp (Middleware f) args = withTempDirectory fp "ytdl" $ \tmp -> do
    youtubeDl' tmp args
    f tmp
    moveFiles tmp fp

-- | Run youtube-dl without middleware.
--
-- Faster than `youtubeDl` because files are directly saved in the destination
-- directory instead of first going through a temporary directory.
youtubeDl' :: FilePath -- ^ Working directory
           -> [String] -- ^ Arguments
           -> IO ()
youtubeDl' fp args = runProcess_
    $ setWorkingDir fp
    $ proc ytdlCommand args

moveFiles :: FilePath -> FilePath -> IO ()
moveFiles src dst = foldDirectoryContents src $ mapM_ $ \x ->
    renamePath (src </> x) (dst </> x)
