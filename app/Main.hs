{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import App.Config
import App.Parser (appParser)
import App.Types
import App.Utils
import qualified Control.Concurrent.Lock as Lock
import Data.Function ((&))
import Database.Redis (ConnectInfo(..), PortID(..), connect, defaultConnectInfo)
import Env (getIntFromEnv, getStringFromEnv)
import Foundation
import Handlers.Home (getHomeR)
import Handlers.Post (getPostR)
import Options.Applicative
import Yesod.Core

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = do
  opts <-
    execParser (info (appParser <**> helper) (fullDesc <> progDesc "Lightweight blog on Haskell!"))
  configRes <- getAppConfig (opts & configPath)
  case configRes of
    (Left err) -> print err
    (Right (AppConfig {..})) -> do
      let postDepthLimit = blogDepthLimit
      redisConnectionPool <-
        connect
          defaultConnectInfo
            {connectHost = redisHost, connectPort = PortNumber (read $ show redisPort)}
      redisWriteLock <- Lock.new
      warp 3000 App {..}
