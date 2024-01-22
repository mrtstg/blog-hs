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
import           Data.Text               (Text, pack)
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
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel=stylesheet href=https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css>
    $maybe siteName'' <- siteName'
        <meta property=og:site_name content="#{siteName''}">
    ^{pageHead pc}
  <body>
    <div .is-flex.is-flex-direction-column style="height:100vh;">
        <nav .navbar.is-primary.p-3 role=navigation>
            <a .navbar-item.has-background-primary href=/>
                <p .title.is-3> Your blog name here
        <section #content .is-flex-grow-1>
            ^{pageBody pc}
        <footer .footer>
            <div .content.has-text-centered>
                <p><b> Your name </b>, 2023
|]
  errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
      setTitle "Not found!"
      toWidget [hamlet|
<section .hero.is-primary.is-fullheight>
    <div .hero-body>
        <div>
            <h1 .title> Not found!
            <p .subtitle> This page does not exists or not available!
|]
  errorHandler e = do
      $logError (pack $ show e)
      fmap toTypedContent $ defaultLayout $ do
        setTitle "Error!"
        toWidget [hamlet|
<section .hero.is-primary.is-fullheight>
    <div .hero-body>
        <div>
            <h1 .tutle> Error!
            <p .subtitle> Something went wrong. Try later, please!
|]
