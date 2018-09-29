{-# LANGUAGE OverloadedStrings #-}
module Project 
    ( parse
    , page
    ) where

import qualified Data.Text as T

import MdToHtml

data Project = Project T.Text T.Text T.Text
    deriving (Show, Read)

deprecated :: Project -> Bool
deprecated (Project _ url _) = T.head url == '#'

parseOne :: T.Text -> Project
parseOne t = Project name url description
    where
    (name:url:descriptionPieces) = T.splitOn "\n" t
    description = T.unlines descriptionPieces

parse :: T.Text -> [Project]
parse = map parseOne . T.splitOn "\n\n"

projectToButton :: Project -> Html
projectToButton p@(Project name url description) = convertMarkdownToHtmlSafe $ T.concat [nameMd, deprecationMd, "  \n", description]
    where
    nameMd = T.concat ["**", name, "**"]
    deprecationMd = if deprecated p then " (Deprecated)" else ""

page :: [Project] -> Html
page ps = RawHtml $ T.concat $ (title:wrappedProjects)
    where
    title = showText $ convertMarkdownToHtmlSafe $ "## Projects"
    wrapProject p@(Project _ url _) = if deprecated p
        then T.concat ["<div>",                  (showText . projectToButton) p , "</div>"]
        else T.concat ["<a href=\"", url, "\">", (showText . projectToButton) p , "</a>"]
    wrappedProjects = map wrapProject ps

