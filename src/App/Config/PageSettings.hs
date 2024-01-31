{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Config.PageSettings
  ( PageSettings(..)
  , PageName(..)
  , defaultPageOpts
  ) where

import qualified Data.Text    as T
import           Data.Yaml
import           GHC.Generics

data PageName = IndexPage | CategoryPage deriving (Eq, Show, Generic)

instance FromJSON PageName where
  parseJSON = withText "PageName" $ \v -> case T.toLower v of
    "index"                                   -> return IndexPage
    v' | v' `elem` ["categories", "category"] -> return CategoryPage
    _                                         -> fail "Invalid page name"

newtype PageSettings = PageSettings [PageName] deriving (Show, Generic)

instance FromJSON PageSettings

defaultPageOpts :: PageSettings
defaultPageOpts = PageSettings []
