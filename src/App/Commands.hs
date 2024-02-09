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
import           App.Config.PostCategoryInfo
import           App.Types
import           App.Utils                   (checkCategoriesPosts,
                                              checkPostInfoFiles,
                                              parsePostInfoFiles)
import qualified Control.Concurrent.Lock     as Lock
import           Control.Monad               (when)
import           Control.Monad.Logger
import           Crud                        (initiatePosts)
import           Data.Text                   (pack)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Redis              (ConnectInfo (..), PortID (..),
                                              connect, defaultConnectInfo)
import           Foundation
import           Handlers.Home               (getHomeR)
import           Handlers.Post               (getPostR)
import           Handlers.PostByCategory     (getCategoryR)
import           Handlers.Robots             (getRobotsR)
import           Handlers.Sitemap            (getSitemapR)
import           System.Directory            (copyFile, removeFile)
import           System.Exit                 (ExitCode (..), exitWith)
import           System.FilePath             (addExtension)
import           Yesod.Core
import           Yesod.Persist               (YesodPersist (runDB))

mkYesodDispatch "App" resourcesApp

runServerCommand :: AppConfig -> Int -> Bool -> IO ()
runServerCommand config@(AppConfig { redisHost = redisHost, redisPort = redisPort, dbPath = dbPath', blogDepthLimit = blogDepthLimit}) port createDatabase = do
  when createDatabase $ runCreateDatabase config
  let postDepthLimit = blogDepthLimit
  redisConnectionPool <-
    connect
      defaultConnectInfo {connectHost = redisHost, connectPort = PortNumber (read $ show redisPort)}
  redisWriteLock <- Lock.new
  let dbPath = pack dbPath'
  warp port App {..}

runCheckFiles :: AppConfig -> IO ()
runCheckFiles AppConfig { postsCategories = categories' } = do
  res <- checkPostInfoFiles "./templates"
  res2 <- checkCategoriesPosts categories' "./templates"
  case (res, res2) of
    (Just _, Just _) -> putStrLn "Everything is ok!"
    _somethingFailed -> exitWith (ExitFailure 3)

runCreateDatabase :: AppConfig -> IO ()
runCreateDatabase (AppConfig {dbPath = dbPath, postsCategories = categories}) = do
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
          runSqlite (pack tmpDBPath) $ do
              runMigration dbMigration
              _ <- insertMany $ map (\(PostCategoryInfo name displayName desc) -> Category name displayName desc) categories
              return ()
          posts <- runNoLoggingT $ withSqliteConn (pack tmpDBPath) $ do
            runSqlConn (initiatePosts lst)
          putStrLn $ "Created " ++ show posts ++ " posts!"
          copyFile tmpDBPath dbPath
          removeFile tmpDBPath

runCommand :: AppCommand -> AppConfig -> AppOpts -> IO ()
runCommand cmd cfg AppOpts { serverPort = port, createDB = createDatabase } =
  case cmd of
    RunServer      -> runServerCommand cfg port createDatabase
    CheckFiles     -> runCheckFiles cfg
    CreateDatabase -> runCreateDatabase cfg
