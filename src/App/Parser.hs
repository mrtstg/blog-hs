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

runServerParser :: Parser AppCommand
runServerParser = pure RunServer

checkFilesParser :: Parser AppCommand
checkFilesParser = pure CheckFiles

appParser :: Parser AppOpts
appParser =
  AppOpts <$>
  optional
    (option
       (str >>= parsePath')
       (long "file" <> short 'f' <> metavar "FILE" <> help "Configuration file path")) <*>
  subparser
    (command "run" (info runServerParser (progDesc "Run server")) <>
     command
       "check"
       (info checkFilesParser (progDesc "Check if meta files for markdown is existing")))
