{-# LANGUAGE OverloadedStrings #-}
module Project 
    ( parseProjects
    , projectsToHtml
    ) where

import qualified Data.Text as T

import MdToHtml

data Project = Project T.Text T.Text T.Text
    deriving (Show, Read)

deprecated :: Project -> Bool
deprecated (Project _ url _) = T.head url == '#'

parseProject :: T.Text -> Project
parseProject t = Project name url description
    where
    (name:url:descriptionPieces) = T.splitOn "\n" t
    description = T.unlines descriptionPieces

parseProjects :: T.Text -> [Project]
parseProjects = map parseProject . T.splitOn "\n\n"

projectToHtml :: Project -> Html
projectToHtml p@(Project name url description) = convertMarkdownToHtmlSafe $ Markdown $ T.concat [nameMd, deprecationMd, "  \n", description]
    where
    nameMd = T.concat ["**", name, "**"]
    deprecationMd = if deprecated p then " (Deprecated)" else ""

projectsToHtml :: [Project] -> Html
projectsToHtml ps = Html . T.concat $ (title:wrappedProjects)
    where
    title = html $ convertMarkdownToHtmlSafe $ Markdown "## Projects"
    wrapProject p@(Project _ url _) = if deprecated p
        then T.concat ["<div>",                  (html . projectToHtml) p , "</div>"]
        else T.concat ["<a href=\"", url, "\">", (html . projectToHtml) p , "</a>"]
    wrappedProjects = map wrapProject ps

