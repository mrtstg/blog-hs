module App.Redis (cacheRedisDataMD5, getCachedRedisDataMD5) where

import qualified Data.ByteString.Char8 as B
import           Data.Hash.MD5         (Str (..), md5s)
import qualified Database.Redis        as R

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
