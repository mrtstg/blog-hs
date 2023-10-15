module Parser.Utils
  ( between'
  , space'
  , space''
  , asterisk'
  , asterisk''
  , codeBlock
  ) where

import Text.Parsec
import Text.Parsec.String

between' :: Parser a -> Parser b -> Parser c -> Parser [c]
between' a b c = a *> manyTill c (try b)

space' :: Parser String
space' = many (char ' ')

space'' :: Parser String
space'' = many1 (char ' ')

asterisk' :: Parser String
asterisk' = count 1 $ char '*'

asterisk'' :: Parser String
asterisk'' = count 2 $ char '*'

codeBlock :: Parser String
codeBlock = count 3 (char '`')
