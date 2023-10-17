{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import qualified Control.Concurrent.Lock as Lock
import Database.Redis (ConnectInfo(..), PortID(..), connect, defaultConnectInfo)
import Env (getIntFromEnv, getStringFromEnv)
import Foundation
import Handlers.Home (getHomeR)
import Handlers.Post (getPostR)
import Yesod.Core

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = do
  postDepthLimit <- getIntFromEnv "DEPTH_LEVEL" 1
  redisHost <- getStringFromEnv "REDIS_HOST" "localhost"
  redisPort <- getIntFromEnv "REDIS_PORT" 6379
  redisConnectionPool <-
    connect
      defaultConnectInfo {connectHost = redisHost, connectPort = PortNumber (read $ show redisPort)}
  redisWriteLock <- Lock.new
  warp 3000 App {..}
