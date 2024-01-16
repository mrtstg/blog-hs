module App.Types
  ( AppOpts(..)
  , AppCommand(..)
  ) where

data AppOpts = AppOpts
  { configPath :: !(Maybe FilePath)
  , serverPort :: !Int
  , appCommand :: !AppCommand
  }

data AppCommand
  = RunServer
  | CheckFiles
  | CreateDatabase
