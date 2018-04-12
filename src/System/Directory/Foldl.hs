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

{-# LANGUAGE BangPatterns #-}

module System.Directory.Foldl
    ( foldDirStream
    , foldDirectoryContents
    ) where

import Control.Monad             ((<=<))
import Control.Foldl             (FoldM, impurely_)
import Control.Exception         (bracket)
import Data.Streaming.Filesystem (DirStream, openDirStream, readDirStream,
                                  closeDirStream)


-- | Fold over the entries of a `DirStream` without the special entries (@.@ and
-- @..@).
foldDirStream :: DirStream -> FoldM IO FilePath a -> IO a
foldDirStream s = impurely_ $ (=<<) . foldDirStream' s

-- | Fold over the entries of a directory without the special entries (@.@ and
-- @..@).
foldDirectoryContents :: FilePath -> FoldM IO FilePath a -> IO a
foldDirectoryContents fp f = bracket (openDirStream fp)
                                     closeDirStream
                                     (`foldDirStream` f)

foldDirStream' :: DirStream -> (x -> FilePath -> IO x) -> x -> IO x
foldDirStream' s step = go
  where
    go !x = maybe (pure $! x) (go <=< step x) =<< readDirStream s
