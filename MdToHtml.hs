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
import qualified Data.Text as T

newtype Html     = Html     { fromHtml :: T.Text }
    deriving (Show, Read)

-- Conversion Systems

convertMarkdownToHtml :: T.Text -> Either PandocError Html
convertMarkdownToHtml t = runPure $ readMarkdown def t >>= writeHtml5String def >>= return . Html

convertMarkdownToHtmlIO :: T.Text -> IO Html
convertMarkdownToHtmlIO t = handleError $ convertMarkdownToHtml t

convertMarkdownToHtmlSafe :: T.Text -> Html
convertMarkdownToHtmlSafe t = case convertMarkdownToHtml t of
    Right html -> html
    Left  _    -> Html "Parsing error in Markdown."

-- Simple HTML DSL
data Attribute = Attribute { key :: T.Text, value :: T.Text }
data Node = Element { name :: T.Text, attributes :: [Attribute], children :: [Node] }
          | TextNode T.Text
          | RawHtml T.Text

showText :: Node -> T.Text
showText (Element name attributes children) = T.concat [openingTag, childrenT, closingTag]
    where
    openingTag  = T.concat $ ["<", name, attributesT, ">"]
    attributesT = T.concat $ map (\(Attribute k v) -> T.concat [" ", k, "=\"", v, "\""]) attributes
    childrenT   = T.concat $ map showText children
    closingTag  = T.concat $ ["</", name, ">"]

showText (TextNode t) = t
showText (RawHtml t) = t

instance Show Node where
    show = T.unpack . showText
