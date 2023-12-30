module Parser.Utils
  ( between'
  , space'
  , space''
  , asterisk'
  , asterisk''
  , codeBlock
  ) where

import           Data.Attoparsec.Combinator (many1)
import           Data.Attoparsec.Text

between' :: Parser a -> Parser b -> Parser c -> Parser [c]
between' a b c = a *> manyTill c (try b)

space' :: Parser String
space' = choice [try $ many1 (char ' '), return ""]

space'' :: Parser String
space'' = many1 (char ' ')

asterisk' :: Parser String
asterisk' = count 1 $ char '*'

asterisk'' :: Parser String
asterisk'' = count 2 $ char '*'

codeBlock :: Parser String
codeBlock = count 3 (char '`')
