{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Config
  ( AppConfig(..)
  , PostCategoryInfo(..)
  , PostRenderSettings(..)
  , defaultPostRenderOpts
  ) where

import           Data.Yaml
import           GHC.Generics

data PostCategoryInfo = PostCategoryInfo
  { postCategoryName        :: !String,
    postCategoryDisplayName :: !String,
    postCategoryDescription :: !String
  } deriving (Generic, Show)

instance FromJSON PostCategoryInfo where
  parseJSON = withObject "PostCategoryInfo" $ \v -> PostCategoryInfo <$> v .: "name" <*> v .: "displayName" <*> v .: "description"

data PostRenderSettings = PostRenderSettings
  { postRenderTitle      :: !Bool,
    postRenderDate       :: !Bool,
    postRenderCategories :: !Bool
  } deriving (Generic, Show)

instance FromJSON PostRenderSettings where
  parseJSON = withObject "PostRenderSettings" $ \v -> PostRenderSettings
    <$> v .:? "renderTitle" .!= True
    <*> v .:? "renderDate" .!= True
    <*> v .:? "renderCategories" .!= True

defaultPostRenderOpts :: PostRenderSettings
defaultPostRenderOpts = PostRenderSettings True True True

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
