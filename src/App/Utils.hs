module App.Utils
  ( getAppConfig
  , listFiles
  , listDatalessFiles
  , checkPostInfoFiles
  , listPostInfoFiles
  , parsePostInfoFiles
  , normaliseFilePath
  ) where

import           App.Config       (AppConfig (..))
import           App.PostInfo     (PostInfo (..), parsePostInfoFromFile)
import           Control.Monad    (forM_)
import           Data.Yaml        (ParseException, decodeFileEither)
import           Env              (getBoolFromEnv, getIntFromEnv,
                                   getStringFromEnv)
import           System.Directory
import           System.FilePath  (addExtension, combine, dropExtension,
                                   isAbsolute, joinPath, normalise, splitPath,
                                   takeExtension)

normaliseFilePath :: FilePath -> FilePath
normaliseFilePath p =
  if isAbsolute p
    then p
    else do
      (joinPath . drop 1 . splitPath . normalise) p

parsePostInfoFiles :: FilePath -> IO (Either ParseException [(FilePath, PostInfo)])
parsePostInfoFiles p = do
  files <- listPostInfoFiles p
  let files' = map (flip addExtension "md" . dropExtension) files
  results <- mapM parsePostInfoFromFile files
  case sequence results of
    (Left e)  -> return $ Left e
    (Right r) -> return (Right $ zip files' r)

checkPostInfoFiles :: FilePath -> IO (Maybe ())
checkPostInfoFiles p = do
  invalidFiles <- listDatalessFiles p
  case invalidFiles of
    [] -> return $ Just ()
    lst -> do
      putStrLn "Following files are without YML file:"
      forM_ lst putStrLn
      return Nothing

listPostInfoFiles :: FilePath -> IO [FilePath]
listPostInfoFiles p = do
  files <- listMarkdownFiles p
  return $ map (flip addExtension ".yml" . dropExtension) files

listMarkdownFiles :: FilePath -> IO [FilePath]
listMarkdownFiles = listFiles f
  where
    f :: FilePath -> IO Bool
    f = return . (== ".md") . takeExtension

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
  enableIndex <- getBoolFromEnv "ENABLE_INDEX" True
  return $
    AppConfig
      { redisHost = redisHost'
      , redisPort = redisPort'
      , blogDepthLimit = postDepthLimit'
      , App.Config.dbPath = dbPath'
      , enableIndexPage = enableIndex
      }

getAppConfigFromFile :: FilePath -> IO (Either String AppConfig)
getAppConfigFromFile p = do
  res' <- decodeFileEither p
  return $
    case res' of
      (Left e)  -> Left $ show e
      (Right r) -> Right r

getAppConfig :: Maybe FilePath -> IO (Either String AppConfig)
getAppConfig p =
  case p of
    Nothing -> do
      Right <$> getAppConfigFromEnv
    (Just v) -> getAppConfigFromFile v
