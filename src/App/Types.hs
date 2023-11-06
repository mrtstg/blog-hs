module App.Types
  ( AppOpts(..)
  , AppCommand(..)
  ) where

data AppOpts = AppOpts
  { configPath :: Maybe FilePath
  , appCommand :: AppCommand
  }

data AppCommand
  = RunServer
  | CheckFiles
  | CreateDatabase
