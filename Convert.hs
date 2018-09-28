{-# LANGUAGE OverloadedStrings #-}
module Convert where

import Text.Pandoc.Class            (runPure)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML     (writeHtml5String)
import Text.Pandoc.Error            (PandocError, handleError)
import Data.Default                 (def)

import qualified Data.Text as T

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
