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
-- Downloads the latest videos from a playlist, but removes UUIDs from any mkv
-- files produced.
-----------------------------------------------------------------------------

import System.Environment (getArgs, getProgName)
import System.Exit        (exitFailure)

import Media.MKV.UUID                (removeUUIDsIn)
import Web.YouTube                   (Middleware (..))
import Web.YouTube.DateFormat.Latest (downloadLatest)


main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go (pl:args) = downloadLatest "." pl args $ Middleware removeUUIDsIn
go []        = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " <playlist> [<args>...]"
    exitFailure
