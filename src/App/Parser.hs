module App.Parser
  ( appParser
  ) where

import App.Types
import Options.Applicative
import System.FilePath

parsePath' :: String -> ReadM FilePath
parsePath' s =
  if isValid s
    then return s
    else readerError $ "Invalid path: " ++ s

appParser :: Parser AppOpts
appParser =
  AppOpts <$>
  optional
    (option
       (str >>= parsePath')
       (long "file" <> short 'f' <> metavar "FILE" <> help "Configuration file path"))
