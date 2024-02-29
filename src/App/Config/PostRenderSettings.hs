{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Config.PostRenderSettings
  ( PostRenderSettings(..)
  , defaultPostRenderOpts
  ) where

import           Data.Yaml
import           GHC.Generics

data PostRenderSettings = PostRenderSettings
  { postRenderTitle      :: !Bool,
    postRenderDate       :: !Bool,
    postRenderCategories :: !Bool,
    postRenderMeta       :: !Bool
  } deriving (Generic, Show)

instance FromJSON PostRenderSettings where
  parseJSON = withObject "PostRenderSettings" $ \v -> PostRenderSettings
    <$> v .:? "renderTitle" .!= True
    <*> v .:? "renderDate" .!= True
    <*> v .:? "renderCategories" .!= True
    <*> v .:? "renderMeta" .!= True

defaultPostRenderOpts :: PostRenderSettings
defaultPostRenderOpts = PostRenderSettings True True True True
