{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import qualified Data.Map as Map
import Database.Redis (Connection)
import Yesod.Core

type TemplatesCacheMap = Map.Map String String

data App = App
  { postDepthLimit :: Int
  , redisConnectionPool :: Connection
  }

mkYesodData
  "App"
  [parseRoutes|
/ HomeR GET
/post/*Texts PostR GET
|]

instance Yesod App
