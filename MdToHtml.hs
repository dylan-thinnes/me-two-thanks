{-# LANGUAGE OverloadedStrings #-}
module MdToHtml 
    ( convertMarkdownToHtml
    , convertMarkdownToHtmlIO
    , convertMarkdownToHtmlSafe
    , Html(..)
    , Markdown(..)
    ) where

import Text.Pandoc.Class            (runPure)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML     (writeHtml5String)
import Text.Pandoc.Error            (PandocError, handleError)
import Data.Default                 (def)
import Data.Text                    (Text)

newtype Markdown = Markdown { fromMd   :: Text }
    deriving (Show, Read)
newtype Html     = Html     { fromHtml :: Text }
    deriving (Show, Read)

convertMarkdownToHtml :: Markdown -> Either PandocError Html
convertMarkdownToHtml t = runPure $ readMarkdown def (fromMd t) >>= writeHtml5String def >>= return . Html

convertMarkdownToHtmlIO :: Markdown -> IO Html
convertMarkdownToHtmlIO t = handleError $ convertMarkdownToHtml t

convertMarkdownToHtmlSafe :: Markdown -> Html
convertMarkdownToHtmlSafe t = case convertMarkdownToHtml t of
    Right html -> html
    Left  _    -> Html "Parsing error in Markdown."
