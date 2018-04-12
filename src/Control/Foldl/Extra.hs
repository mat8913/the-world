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

module Control.Foldl.Extra
    ( collectLargest
    ) where

import Control.Foldl (Fold (..))
import Data.Monoid   ((<>))


-- | Collect the largest elements in a `Monoid`.
--
-- @'collectLargest' fo fm@ returns @'foldMap' fm@ over the largest elements
-- according to @'Data.Ord.comparing' fo@.
collectLargest :: (Ord o, Monoid m) => (a -> o) -> (a -> m) -> Fold a m
collectLargest fo fm = Fold step (Nothing, mempty) snd
  where
    step (Nothing, _) x = (Just $ fo x, fm x)
    step (Just !o, !m) x = case compare o o' of
        LT -> (Just o', fm x)
        EQ -> (Just o , m <> fm x)
        GT -> (Just o , m)
      where
        o' = fo x
