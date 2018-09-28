{-# LANGUAGE OverloadedStrings #-}
module Convert where

import Text.Pandoc.Class            (runPure)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML     (writeHtml5String)
import Text.Pandoc.Error            (PandocError, handleError)
import Data.Default                 (def)

import qualified Data.Text as T
import Data.Maybe (fromMaybe)

newtype Markdown = Markdown { md   :: T.Text }
    deriving (Show, Read)
newtype Html     = Html     { html :: T.Text }
    deriving (Show, Read)

convertMarkdownToHtml :: Markdown -> Either PandocError Html
convertMarkdownToHtml t = runPure $ readMarkdown def (md t) >>= writeHtml5String def >>= return . Html

convertMarkdownToHtmlIO :: Markdown -> IO Html
convertMarkdownToHtmlIO t = handleError $ convertMarkdownToHtml t

convertMarkdownToHtmlSafe :: Markdown -> Html
convertMarkdownToHtmlSafe t = case convertMarkdownToHtml t of
    Right html -> html
    Left html  -> Html "Parsing error in Markdown."

data Article = Article T.Text (Maybe T.Text) (Maybe T.Text) (Maybe Markdown) Markdown
    deriving (Show, Read)

getMetadata :: T.Text -> [T.Text]
getMetadata = takeWhile (/="") . T.lines

getBody :: T.Text -> T.Text
getBody = T.tail . T.tail . snd . T.breakOn "\n\n"

indexJust :: [a] -> Int -> Maybe a
indexJust vs i = if length vs > i then Just $ vs !! i else Nothing

parseArticle :: T.Text -> Article
parseArticle t = Article title author date description content
    where
    metadata = getMetadata t
    content = Markdown $ getBody t
    title = metadata !! 0
    author      = indexJust metadata 1
    date        = indexJust metadata 2
    description = Markdown <$> indexJust metadata 3

articleToHtml :: Article -> Html
articleToHtml (Article title author date description content) = convertMarkdownToHtmlSafe $ Markdown $ T.concat [headerMd, "\n\n", md content]
    where
    headerMd = T.concat [titleMd, "\n*", authorMd, dateMd, "*"]
    titleMd  = T.append "#" title
    authorMd = fromMaybe "Anonymous" author
    dateMd   = fromMaybe "" $ fmap (T.append ", ") date

articleToBlurb :: Article -> Html
articleToBlurb (Article title author date description content) = convertMarkdownToHtmlSafe $ Markdown $ T.concat [headerMd, descriptionMd]
    where
    descriptionMd = fromMaybe "" $ fmap (T.append "  \n" . md) description
    headerMd      = T.concat [titleMd, "  \n*", authorMd, dateMd, "*"]
    titleMd       = T.concat ["**", title, "**"]
    authorMd      = fromMaybe "Anonymous" author
    dateMd        = fromMaybe "" $ fmap (T.append ", ") date

data Project = Project T.Text T.Text T.Text
    deriving (Show, Read)

parseProject :: T.Text -> Project
parseProject t = Project name url description
    where
    (name:url:descriptionPieces) = T.splitOn "\n" t
    description = T.unlines descriptionPieces

parseProjects :: T.Text -> [Project]
parseProjects = map parseProject . T.splitOn "\n\n"

projectToHtml :: Project -> Html
projectToHtml (Project name url description) = convertMarkdownToHtmlSafe $ Markdown $ T.concat [nameMd, "  \n", description]
    where
    nameMd = T.concat ["**", name, "**"]

projectsToHtml :: [Project] -> Html
projectsToHtml ps = Html . T.concat $ (title:wrappedProjects)
    where
    title = html $ convertMarkdownToHtmlSafe $ Markdown "## Projects"
    wrapProject p@(Project _ url _) = T.concat ["<a href=\"", url, "\">", (html . projectToHtml) p , "</a>"]
    wrappedProjects = map wrapProject ps
