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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Web.YouTube.DateFormat
    ( Video (..)
    , format
    , parse
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), evalStateT, get)
import Control.Arrow             (second)
import Control.Lens.Iso          (AnIso', withIso, reversed)

import Data.Time.Calendar (Day, fromGregorianValid)
import Text.Read          (readMaybe)


-- | Represents information parsed from a video filename.
data Video = Video { uploadDate :: Day
                   , title :: String
                   , videoId :: String
                   , ext :: String
                   }
  deriving Show

-- | The output template to be passed to youtube-dl via the --output option.
format :: String
format = "%(upload_date)s - %(title)s - %(id)s.%(ext)s"

-- | Parse a filename produced by youtube-dl with the above output template.
parse :: String -> Maybe Video
parse = evalStateT $ do
    uploadDate <- parseUploadDate
    ext <- parseExt
    videoId <- parseVideoId
    title <- get
    pure Video {..}

parseUploadDate :: StateT String Maybe Day
parseUploadDate = do
    y <- lift . readMaybe =<< parseN 4
    m <- lift . readMaybe =<< parseN 2
    d <- lift . readMaybe =<< parseN 2
    parseSep
    lift $ fromGregorianValid y m d

parseExt :: StateT String Maybe String
parseExt = withStateTIso reversed $ do
    xs <- StateT $ Just . break (=='.')
    c <- parseChar
    if c == '.'
      then pure $ reverse xs
      else lift Nothing

parseVideoId :: StateT String Maybe String
parseVideoId = withStateTIso reversed $ do
    xs <- StateT $ Just . break (==' ')
    parseSep
    pure $ reverse xs

parseSep :: StateT String Maybe ()
parseSep = do
    sep' <- parseN 3
    if sep' == " - "
      then pure ()
      else lift Nothing

parseN :: Int -> StateT [a] Maybe [a]
parseN n = do
    xs <- StateT $ Just . splitAt n
    if length xs == n
      then pure xs
      else StateT $ const Nothing

parseChar :: StateT [a] Maybe a
parseChar = StateT $ \case
    []     -> Nothing
    (x:xs) -> Just (x, xs)

withStateTIso :: Functor m => AnIso' s t -> StateT t m a -> StateT s m a
withStateTIso iso (StateT f) = withIso iso $ \to from ->
    StateT $ \s -> second from <$> f (to s)
