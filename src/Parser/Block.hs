module Parser.Block
  ( parseBlocks
  ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (guard)
import           Data.Attoparsec.Text
import           Data.Functor         ((<&>))
import           Parser.Inline
import           Parser.Types
import           Parser.Utils

parseBlocks :: Parser [MarkdownBlock]
parseBlocks = manyTill parseBlock endOfInput

parseBlock :: Parser MarkdownBlock
parseBlock = choice [try parseHeader, try parseQuote, try parseList, try parseCode, parseParagraph]

parseOrderedList :: Parser MarkdownBlock
parseOrderedList = many1 listItem <&> List OrderedList
  where
    listItem = ((many1 digit >> char '.') <* space') >> parseMarkdownInlines

unorderedListSyms :: Parser Char
unorderedListSyms = choice [char '-', char '+', char '*']

parseUnorderedList :: Parser MarkdownBlock
parseUnorderedList = many1 listItem <&> List UnorderedList
  where
    listItem = (many1 unorderedListSyms <* space'') >> parseMarkdownInlines

parseList :: Parser MarkdownBlock
parseList = parseUnorderedList <|> parseOrderedList

parseQuote :: Parser MarkdownBlock
parseQuote = (char '>' <* skipMany1 (char ' ')) >> (parseMarkdownInlines <&> Quote)

parseParagraph :: Parser MarkdownBlock
parseParagraph = Paragraph <$> parseMarkdownInlines

parseCode :: Parser MarkdownBlock
parseCode = do
  _ <- codeBlock
  language <- between' space' (space' <* endOfLine) anyChar
  code <- manyTill anyChar codeBlock
  _ <- space'
  _ <- endOfLine
  return $
    Code
      (if null language
         then Nothing
         else Just language)
      code

parseHeader :: Parser MarkdownBlock
parseHeader = do
  symbols <- many1 $ char '#'
  let headerL = length symbols
  guard (headerL <= 6)
  skipMany1 (char ' ')
  Header headerL <$> parseMarkdownInlines
