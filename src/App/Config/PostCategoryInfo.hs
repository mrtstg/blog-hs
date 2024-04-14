{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Config.PostCategoryInfo
  ( PostCategoryInfo(..)
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
