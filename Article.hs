{-# LANGUAGE OverloadedStrings #-}
module Article 
    ( parse
    , longform
    , blurb
    ) where

import MdToHtml
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

data Article = Article 
    { title       :: T.Text
    , author      :: Maybe T.Text
    , date        :: Maybe T.Text
    , description :: Maybe T.Text
    , content     :: T.Text
    }
    deriving (Show, Read)

-- Utility Functions for Parsing in Files

getMetadata :: T.Text -> [T.Text]
getMetadata = takeWhile (/="") . T.lines

getBody :: T.Text -> T.Text
getBody = T.tail . T.tail . snd . T.breakOn "\n\n"

indexJust :: [a] -> Int -> Maybe a
indexJust vs i = if length vs > i then Just $ vs !! i else Nothing

parse :: T.Text -> Article
parse t = Article title author date description content
    where
    metadata = getMetadata t
    content = getBody t
    title = metadata !! 0
    author      = indexJust metadata 1
    date        = indexJust metadata 2
    description = indexJust metadata 3

longform :: Article -> Html
longform (Article title author date description content) = convertMarkdownToHtmlSafe $ T.concat [headerMd, "\n\n", content]
    where
    headerMd = T.concat [titleMd, "\n*", authorMd, dateMd, "*"]
    titleMd  = T.append "#" title
    authorMd = fromMaybe "Anonymous" author
    dateMd   = fromMaybe "" $ fmap (T.append ", ") date

blurb :: Article -> Html
blurb (Article title author date description content) = convertMarkdownToHtmlSafe $ T.concat [headerMd, descriptionMd]
    where
    descriptionMd = fromMaybe "" $ fmap (T.append "  \n") description
    headerMd      = T.concat [titleMd, "  \n*", authorMd, dateMd, "*"]
    titleMd       = T.concat ["**", title, "**"]
    authorMd      = fromMaybe "Anonymous" author
    dateMd        = fromMaybe "" $ fmap (T.append ", ") date
