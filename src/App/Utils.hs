module App.Utils
  ( getAppConfig
  , listFiles
  , listDatalessFiles
  ) where

import App.Config (AppConfig(..))
import Data.Yaml (decodeFileEither)
import Env (getIntFromEnv, getStringFromEnv)
import System.Directory
import System.FilePath (addExtension, combine, dropExtension, takeExtension)

listDatalessFiles :: FilePath -> IO [FilePath]
listDatalessFiles = listFiles f
  where
    f :: FilePath -> IO Bool
    f p = do
      if takeExtension p /= ".md"
        then return False
        else do
          let nPath = addExtension (dropExtension p) ".yml"
          v <- doesFileExist nPath
          return $ not v

listFiles :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
listFiles pred' path = do
  isDir <- doesDirectoryExist path
  if not isDir
    then do
      valid <- pred' path
      return [path | valid]
    else do
      files <- listDirectory path
      lst <- traverse (listFiles pred' . combine path) files
      return $ concat lst

getAppConfigFromEnv :: IO AppConfig
getAppConfigFromEnv = do
  postDepthLimit' <- getIntFromEnv "DEPTH_LEVEL" 1
  redisHost' <- getStringFromEnv "REDIS_HOST" "localhost"
  redisPort' <- getIntFromEnv "REDIS_PORT" 6379
  dbPath' <- getStringFromEnv "DB_PATH" "./blog.db"
  return $
    AppConfig
      { redisHost = redisHost'
      , redisPort = redisPort'
      , blogDepthLimit = postDepthLimit'
      , dbPath = dbPath'
      }

getAppConfigFromFile :: FilePath -> IO (Either String AppConfig)
getAppConfigFromFile p = do
  res' <- decodeFileEither p
  return $
    case res' of
      (Left e) -> Left $ show e
      (Right r) -> Right r

getAppConfig :: Maybe FilePath -> IO (Either String AppConfig)
getAppConfig p =
  case p of
    Nothing -> do
      Right <$> getAppConfigFromEnv
    (Just v) -> getAppConfigFromFile v
