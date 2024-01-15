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
  , siteName        :: !(Maybe String)
  , siteHost        :: !(Maybe String)
  , robotsFilePath  :: !(Maybe String)
  } deriving (Generic, Show)

instance FromJSON AppConfig
