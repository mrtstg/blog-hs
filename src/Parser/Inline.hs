{-# LANGUAGE OverloadedStrings #-}

module Parser.Inline
  ( parseMarkdownInlines
  , parseMarkdownInline
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Combinator (lookAhead, many1, manyTill)
import Data.Attoparsec.Text
import Data.Functor ((<&>))
import Parser.Types
import Parser.Utils

parseMarkdownInlines :: Parser [MarkdownInline]
parseMarkdownInlines = manyTill parseMarkdownInline endOfLine

parseMarkdownInline :: Parser MarkdownInline
parseMarkdownInline =
  choice [try parseLink, try parseImage, try parseBold, try parseItalic, parseText]

textStopSyms :: Parser ()
textStopSyms = do
  _ <- choice [char '!', char '[', char '*', char '@']
  return ()

parseText :: Parser MarkdownInline
parseText = do
  c <- anyChar
  cs <- manyTill anyChar $ lookAhead (textStopSyms <|> endOfLine)
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
parseBold = between' asterisk'' asterisk'' parseMarkdownInline <&> Bold

parseItalic :: Parser MarkdownInline
parseItalic = between' asterisk' asterisk' parseMarkdownInline <&> Italic
