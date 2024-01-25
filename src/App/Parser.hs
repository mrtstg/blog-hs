module App.Parser
  ( appParser
  ) where

import           App.Types
import           Options.Applicative
import           System.FilePath

parsePath' :: String -> ReadM FilePath
parsePath' s =
  if isValid s
    then return s
    else readerError $ "Invalid path: " ++ s

runServerParser :: Parser AppCommand
runServerParser = pure RunServer

checkFilesParser :: Parser AppCommand
checkFilesParser = pure CheckFiles

createDatabaseParser :: Parser AppCommand
createDatabaseParser = pure CreateDatabase

appParser :: Parser AppOpts
appParser =
  AppOpts <$>
  optional
    (option
       (str >>= parsePath')
       (long "file" <> short 'f' <> metavar "FILE" <> help "Configuration file path")) <*>
  option auto (long "port" <> short 'p' <> metavar "PORT" <> help "Server port to listen" <> value 3000) <*>
  switch (long "create-db" <> short 'c' <> help "Create SQLite database before starting the server") <*>
  subparser
    (command "run" (info runServerParser (progDesc "Run server")) <>
     command
       "check"
       (info checkFilesParser (progDesc "Check if meta files for markdown is existing")) <>
     command
       "create-db"
       (info createDatabaseParser (progDesc "Create database for blog pages info")))
