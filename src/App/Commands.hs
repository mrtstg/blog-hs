{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module App.Commands
  ( runCommand
  ) where

import App.Config
import App.Types
import App.Utils (listDatalessFiles)
import qualified Control.Concurrent.Lock as Lock
import Control.Monad (forM_)
import Database.Redis (ConnectInfo(..), PortID(..), connect, defaultConnectInfo)
import Foundation
import Handlers.Home (getHomeR)
import Handlers.Post (getPostR)
import System.Exit (ExitCode(..), exitWith)
import Yesod.Core

mkYesodDispatch "App" resourcesApp

runServerCommand :: AppConfig -> IO ()
runServerCommand (AppConfig {..}) = do
  let postDepthLimit = blogDepthLimit
  redisConnectionPool <-
    connect
      defaultConnectInfo {connectHost = redisHost, connectPort = PortNumber (read $ show redisPort)}
  redisWriteLock <- Lock.new
  warp 3000 App {..}

runCheckFiles :: AppConfig -> IO ()
runCheckFiles _ = do
  invalidFiles <- listDatalessFiles "./templates"
  case invalidFiles of
    [] -> putStrLn "Everything is ok!"
    lst -> do
      putStrLn "Following files are without YML file:"
      forM_ lst putStrLn
      exitWith (ExitFailure 3)

runCommand :: AppCommand -> AppConfig -> IO ()
runCommand cmd cfg =
  case cmd of
    RunServer -> runServerCommand cfg
    CheckFiles -> runCheckFiles cfg
