{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Foundation where

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
  }

mkYesodData
  "App"
  [parseRoutes|
/ HomeR GET
/post/*Texts PostR GET
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
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing
