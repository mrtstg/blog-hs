{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module App.Commands
  ( runCommand
  ) where

import           App.Config
import           App.PostInfo            (PostInfo (..))
import           App.Types
import           App.Utils               (checkPostInfoFiles, listDatalessFiles,
                                          normaliseFilePath, parsePostInfoFiles)
import qualified Control.Concurrent.Lock as Lock
import           Control.Monad           (forM_, when)
import           Data.Function           ((&))
import           Data.Text               (pack)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Redis          (ConnectInfo (..), PortID (..),
                                          connect, defaultConnectInfo)
import           Foundation
import           Handlers.Home           (getHomeR)
import           Handlers.Post           (getPostR)
import           System.Directory        (doesFileExist, removeFile)
import           System.Exit             (ExitCode (..), exitWith)
import           Yesod.Core

mkYesodDispatch "App" resourcesApp

runServerCommand :: AppConfig -> IO ()
runServerCommand (AppConfig redisHost redisPort dbPath' blogDepthLimit) = do
  let postDepthLimit = blogDepthLimit
  redisConnectionPool <-
    connect
      defaultConnectInfo {connectHost = redisHost, connectPort = PortNumber (read $ show redisPort)}
  redisWriteLock <- Lock.new
  let dbPath = pack dbPath'
  warp 3000 App {..}

runCheckFiles :: AppConfig -> IO ()
runCheckFiles _ = do
  res <- checkPostInfoFiles "./templates"
  case res of
    (Just _) -> putStrLn "Everything is ok!"
    Nothing  -> exitWith (ExitFailure 3)

runCreateDatabase :: AppConfig -> IO ()
runCreateDatabase (AppConfig {dbPath = dbPath}) = do
  dbExists <- doesFileExist dbPath
  when dbExists $ removeFile dbPath
  filesCheckRes <- checkPostInfoFiles "./templates"
  case filesCheckRes of
    Nothing -> exitWith (ExitFailure 3)
    (Just _) -> do
      parseResults <- parsePostInfoFiles "./templates"
      case parseResults of
        (Left err) -> do
          putStrLn $ "Parse error: " ++ show err
          exitWith (ExitFailure 2)
        (Right lst) -> do
          runSqlite (pack dbPath) $ do
            runMigration dbMigration
            postIds <-
              insertMany $
              map (\(f, l) -> Post (normaliseFilePath f) (l & name) (l & description)) lst
            liftIO $ putStrLn $ "Created " ++ show (length postIds) ++ " posts!"

runCommand :: AppCommand -> AppConfig -> IO ()
runCommand cmd cfg =
  case cmd of
    RunServer      -> runServerCommand cfg
    CheckFiles     -> runCheckFiles cfg
    CreateDatabase -> runCreateDatabase cfg
