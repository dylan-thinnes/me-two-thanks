{-# LANGUAGE OverloadedStrings #-}
module MdToHtml 
    ( convertMarkdownToHtml
    , convertMarkdownToHtmlIO
    , convertMarkdownToHtmlSafe
    , Html(..)
    ) where

import Text.Pandoc.Class            (runPure)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML     (writeHtml5String)
import Text.Pandoc.Error            (PandocError, handleError)
import Data.Default                 (def)
import Data.Text                    (Text)

newtype Html     = Html     { fromHtml :: Text }
    deriving (Show, Read)

-- Conversion Systems

convertMarkdownToHtml :: Text -> Either PandocError Html
convertMarkdownToHtml t = runPure $ readMarkdown def t >>= writeHtml5String def >>= return . Html

convertMarkdownToHtmlIO :: Text -> IO Html
convertMarkdownToHtmlIO t = handleError $ convertMarkdownToHtml t

convertMarkdownToHtmlSafe :: Text -> Html
convertMarkdownToHtmlSafe t = case convertMarkdownToHtml t of
    Right html -> html
    Left  _    -> Html "Parsing error in Markdown."

-- Simple HTML DSL
data Attribute = Attribute { key :: Text, value :: Text }
data Node = Element { name :: Text, attributes :: [Attribute], children :: [Node] }
          | TextNode Text
