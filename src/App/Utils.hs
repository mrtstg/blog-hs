module App.Utils
  ( getAppConfig
  ) where

import App.Config (AppConfig(..))
import Data.Yaml (decodeFileEither)
import Env (getIntFromEnv, getStringFromEnv)

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
