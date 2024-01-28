{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Config
  ( AppConfig(..),
    PostCategoryInfo(..),
  ) where

import           Data.Yaml
import           GHC.Generics

data PostCategoryInfo = PostCategoryInfo
  { postCategoryName        :: !String,
    postCategoryDescription :: !String
  } deriving (Generic, Show)

instance FromJSON PostCategoryInfo where
    parseJSON = withObject "PostCategoryInfo" $ \v -> PostCategoryInfo <$> v .: "name" <*> v .: "description"

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
