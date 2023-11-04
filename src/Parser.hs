module Parser
  ( parseMarkdown
  ) where

import Data.Attoparsec.Text
import Data.Text (pack)
import Parser.Block
import Parser.Types

parseMarkdown :: String -> Either String [MarkdownBlock]
parseMarkdown txt = parseOnly parseBlocks $ pack txt
