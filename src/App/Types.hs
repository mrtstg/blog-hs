module App.Types
  ( AppOpts(..)
  ) where

newtype AppOpts = AppOpts
  { configPath :: Maybe FilePath
  }
