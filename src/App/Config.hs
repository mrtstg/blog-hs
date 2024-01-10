{-# LANGUAGE DeriveGeneric #-}

module App.Config
  ( AppConfig(..)
  ) where

import           Data.Yaml
import           GHC.Generics

data AppConfig = AppConfig
  { redisHost       :: !String
  , redisPort       :: !Int
  , dbPath          :: !String
  , blogDepthLimit  :: !Int
  , enableIndexPage :: !Bool
  } deriving (Generic, Show)

instance FromJSON AppConfig
