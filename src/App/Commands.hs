{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module App.Commands
  ( runCommand
  ) where

import           App.Config
import           App.PostInfo            (PostInfo (..))
import           App.Types
import           App.Utils               (checkPostInfoFiles, listDatalessFiles,
                                          normaliseFilePath, parsePostInfoFiles)
import qualified Control.Concurrent.Lock as Lock
import           Data.Function           ((&))
import           Data.Text               (pack)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Redis          (ConnectInfo (..), PortID (..),
                                          connect, defaultConnectInfo)
import           Foundation
import           Handlers.Home           (getHomeR)
import           Handlers.Post           (getPostR)
import           System.Directory        (copyFile, doesFileExist, removeFile)
import           System.Exit             (ExitCode (..), exitWith)
import           System.FilePath         (addExtension)
import           Yesod.Core

mkYesodDispatch "App" resourcesApp

runServerCommand :: AppConfig -> IO ()
runServerCommand config@(AppConfig { redisHost = redisHost, redisPort = redisPort, dbPath = dbPath', blogDepthLimit = blogDepthLimit}) = do
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
          let tmpDBPath = addExtension dbPath "tmp"
          runSqlite (pack $ tmpDBPath) $ do
            runMigration dbMigration
            postIds <-
              insertMany $
              map (\(f, l) -> Post (normaliseFilePath f) (l & name) (l & description) (l & date)) lst
            liftIO $ putStrLn $ "Created " ++ show (length postIds) ++ " posts!"
          copyFile tmpDBPath dbPath
          removeFile tmpDBPath

runCommand :: AppCommand -> AppConfig -> IO ()
runCommand cmd cfg =
  case cmd of
    RunServer      -> runServerCommand cfg
    CheckFiles     -> runCheckFiles cfg
    CreateDatabase -> runCreateDatabase cfg
