module App.Redis ( cacheRedisDataMD5
  , getCachedRedisDataMD5
  , ParseableCachedData(..)
  , cacheIOJsonData
  , getLockCachedParseableData
) where

import           Control.Concurrent      (threadDelay)
import qualified Control.Concurrent.Lock as Lock
import           Data.Aeson              (FromJSON, ToJSON)
import qualified Data.Aeson              as JSON
import qualified Data.ByteString.Char8   as B
import           Data.ByteString.Lazy    (fromStrict, toStrict)
import           Data.Hash.MD5           (Str (..), md5s)
import qualified Database.Redis          as R

data ParseableCachedData t = RawData !B.ByteString | ParsedData !t

getLockCachedParseableData :: (FromJSON t, ToJSON t) => Lock.Lock -> R.Connection -> String -> IO (Either String t) -> IO (Either String (ParseableCachedData t))
getLockCachedParseableData lock conn key cacheValue = let
  cacheData :: IO (Either String (ParseableCachedData t))
  cacheData = do
    locked' <- Lock.locked lock
    if locked' then do
      _ <- threadDelay 50000
      cacheData
    else do
      Lock.with lock $ cacheIOJsonData conn key cacheValue
  in do
    v <- getCachedRedisDataMD5 conn key
    case v of
      Nothing -> cacheData
      (Just v') -> do
        let parsedData = JSON.decode (fromStrict v')
        case parsedData of
          (Just r) -> return $ Right (ParsedData r)
          Nothing  -> return $ Left "Failed to decode JSON"

cacheIOJsonData :: (ToJSON t) => R.Connection -> String -> IO (Either e t) -> IO (Either e (ParseableCachedData d))
cacheIOJsonData conn path cacheValue = do
  cacheValue' <- cacheValue
  case cacheValue' of
    (Left e) -> return $ Left e
    (Right v) -> do
      let jsonString = toStrict $ JSON.encode v
      cacheRedisDataMD5 conn path jsonString
      return $ Right (RawData jsonString)

getCachedRedisDataMD5 :: R.Connection -> String -> IO (Maybe B.ByteString)
getCachedRedisDataMD5 conn key = do
  let hash = B.pack $ md5s (Str key)
  v <- R.runRedis conn $ do R.get hash
  case v of
    (Left _) -> return Nothing
    (Right res) -> case res of
      Nothing   -> return Nothing
      (Just v') -> return (Just v')

cacheRedisDataMD5 :: R.Connection -> String -> B.ByteString -> IO ()
cacheRedisDataMD5 conn key = cacheRedisData conn (B.pack $ md5s (Str key))

cacheRedisData :: R.Connection -> B.ByteString -> B.ByteString -> IO ()
cacheRedisData conn key data' = do
  _ <- R.runRedis conn $ R.setOpts key data' (R.SetOpts (Just 60) Nothing Nothing)
  return ()
