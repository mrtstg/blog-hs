module Parser
  ( parseMarkdown
  ) where

import Parser.Block
import Parser.Types
import Text.Parsec

parseMarkdown :: String -> Either ParseError [MarkdownBlock]
parseMarkdown = parse parseBlocks ""
