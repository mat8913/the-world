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
    ( ytdlCommand
    , youtubeDl
    ) where

import System.Process.Typed (runProcess_, setWorkingDir, proc)


-- | The youtube-dl command.
ytdlCommand :: FilePath
ytdlCommand = "youtube-dl"

-- | Run youtube-dl.
youtubeDl :: FilePath -- ^ Working directory
          -> [String] -- ^ Arguments
          -> IO ()
youtubeDl fp args = runProcess_
    $ setWorkingDir fp
    $ proc ytdlCommand args
