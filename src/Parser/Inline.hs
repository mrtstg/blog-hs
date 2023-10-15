module Parser.Inline
  ( parseMarkdownInlines
  , parseMarkdownInline
  ) where

import Data.Functor ((<&>))
import Parser.Types
import Parser.Utils
import Text.Parsec
import Text.Parsec.String

parseMarkdownInlines :: Parser [MarkdownInline]
parseMarkdownInlines = manyTill parseMarkdownInline endOfLine

parseMarkdownInline :: Parser MarkdownInline
parseMarkdownInline =
  choice [try parseLink, try parseImage, try parseItalic, try parseBold, parseText]

parseText :: Parser MarkdownInline
parseText = do
  c <- anyChar
  cs <- manyTill anyChar (lookAhead $ oneOf "![*@" <|> endOfLine)
  return $ Text (c : cs)

parseLink :: Parser MarkdownInline
parseLink = do
  name <- between' (char '[') (char ']') anyChar
  address <- between' (char '(') (char ')') anyChar
  return $ AbsoluteLink name address

parseImage :: Parser MarkdownInline
parseImage = do
  alt <- between' (string "![") (char ']') anyChar
  href <- between' (char '(') (char ')') anyChar
  return $ Image alt href

parseBold :: Parser MarkdownInline
parseBold = between' asterisk' asterisk' parseMarkdownInline <&> Bold

parseItalic :: Parser MarkdownInline
parseItalic = between' asterisk'' asterisk'' parseMarkdownInline <&> Italic
