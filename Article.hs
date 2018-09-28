{-# LANGUAGE OverloadedStrings #-}
module Article 
    ( parse
    , longform
    , blurb
    ) where

import MdToHtml
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

data Article = Article T.Text (Maybe T.Text) (Maybe T.Text) (Maybe Markdown) Markdown
    deriving (Show, Read)

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
    content = Markdown $ getBody t
    title = metadata !! 0
    author      = indexJust metadata 1
    date        = indexJust metadata 2
    description = Markdown <$> indexJust metadata 3

longform :: Article -> Html
longform (Article title author date description content) = convertMarkdownToHtmlSafe $ Markdown $ T.concat [headerMd, "\n\n", fromMd content]
    where
    headerMd = T.concat [titleMd, "\n*", authorMd, dateMd, "*"]
    titleMd  = T.append "#" title
    authorMd = fromMaybe "Anonymous" author
    dateMd   = fromMaybe "" $ fmap (T.append ", ") date

blurb :: Article -> Html
blurb (Article title author date description content) = convertMarkdownToHtmlSafe $ Markdown $ T.concat [headerMd, descriptionMd]
    where
    descriptionMd = fromMaybe "" $ fmap (T.append "  \n" . fromMd) description
    headerMd      = T.concat [titleMd, "  \n*", authorMd, dateMd, "*"]
    titleMd       = T.concat ["**", title, "**"]
    authorMd      = fromMaybe "Anonymous" author
    dateMd        = fromMaybe "" $ fmap (T.append ", ") date
