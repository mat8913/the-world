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

module Media.MKV.UUID
    ( removeUUID
    , removeUUIDsIn
    ) where

import Prelude hiding (mapM_)

import Control.Foldl (mapM_, prefilterM)

import System.Directory.Foldl (foldDirectoryContents)
import System.FilePath        ((</>), isExtensionOf)
import System.Process.Typed   (runProcess_, proc)


mkvpropedit :: FilePath
mkvpropedit = "mkvpropedit"

-- | Remove the @segment-uid@ property from an MKV file.
removeUUID :: FilePath -> IO ()
removeUUID fp = runProcess_ $ proc mkvpropedit
    ["--edit", "info", "--delete", "segment-uid", "." </> fp]

-- | Run removeUUID on *.mkv in a directory.
removeUUIDsIn :: FilePath -> IO ()
removeUUIDsIn fp = foldDirectoryContents fp
    $ prefilterM (pure . isExtensionOf "mkv")
    $ mapM_ $ \x -> removeUUID $ fp </> x
