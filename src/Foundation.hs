{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Foundation where

import           App.Config              (AppConfig (..))
import           Control.Concurrent.Lock (Lock)
import           Data.Text               (Text)
import           Data.Time.Calendar      (Day)
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Database.Redis          (Connection)
import           Yesod.Core

data App = App
  { postDepthLimit      :: !Int
  , redisConnectionPool :: !Connection
  , redisWriteLock      :: !Lock
  , dbPath              :: !Text
  , config              :: !AppConfig
  }

mkYesodData
  "App"
  [parseRoutes|
/ HomeR GET
/post/*Texts PostR GET
/category/#Text CategoryR GET
/sitemap.xml SitemapR GET
/robots.txt RobotsR GET
|]

share
  [mkPersist sqlSettings, mkMigrate "dbMigration"]
  [persistLowerCase|
Post
  file String
  title String
  descriptinon String
  date Day
  UniquePostFile file
  deriving Show
PostPhoto
  url String
  post PostId
  UniquePostPhoto post url
  deriving Show
Category
  name String
  displayName String
  description String
  UniqueCategory name
PostCategory
  post PostId
  category CategoryId
  UniquePostCategory post category
|]


instance Yesod App where
  makeSessionBackend _ = return Nothing
  defaultLayout widget = do
    App { .. } <- getYesod
    let siteName' = siteName config
    pc <- widgetToPageContent widget
    withUrlRenderer [hamlet|
$doctype 5
<html prefix="og: http://ogp.me/ns#">
  <head>
    <title> #{pageTitle pc}
    <meta charset=utf-8>
    $maybe siteName'' <- siteName'
        <meta property=og:site_name content="#{siteName''}">
    ^{pageHead pc}
  <body>
    ^{pageBody pc}
|]
  errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
      setTitle "Not found!"
      toWidget [hamlet|
<h1> Page is not found
|]
  errorHandler other    = defaultErrorHandler other
