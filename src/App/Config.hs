{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Config
  ( AppConfig(..)
  ) where

import           App.Config.PostCategoryInfo
import           App.Config.PostRenderSettings
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
  , postsCategories :: ![PostCategoryInfo]
  , renderSettings  :: !PostRenderSettings
  } deriving (Generic, Show)

instance FromJSON AppConfig where
  parseJSON = withObject "AppConfig" $ \v -> AppConfig
    <$> v .: "redisHost"
    <*> v .: "redisPort"
    <*> v .: "dbPath"
    <*> v .: "blogDepthLimit"
    <*> v .: "enableIndexPage"
    <*> v .:? "siteName"
    <*> v .:? "siteHost"
    <*> v .:? "robotsFilePath"
    <*> v .:? "categories" .!= []
    <*> v .:? "renderSettings" .!= defaultPostRenderOpts
