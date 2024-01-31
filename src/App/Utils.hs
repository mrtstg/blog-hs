{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module App.Utils
  ( getAppConfig
  , listFiles
  , listDatalessFiles
  , checkPostInfoFiles
  , listPostInfoFiles
  , parsePostInfoFiles
  , normaliseFilePath
  , processPostPathParts
  , generatePostUrlFromRelativeFile
  , checkCategoriesPosts
  , urlEncodeString
  ) where

import           App.Config                    (AppConfig (..))
import           App.Config.PageSettings       (PageName (..),
                                                PageSettings (..))
import           App.Config.PostCategoryInfo   (PostCategoryInfo (..))
import           App.Config.PostRenderSettings (defaultPostRenderOpts)
import           App.PostInfo                  (PostInfo (..),
                                                parsePostInfoFromFile)
import           Control.Monad                 (forM_)
import           Data.ByteString.Char8         (pack, unpack)
import           Data.List                     (intercalate)
import qualified Data.Text                     as T
import           Data.Yaml                     (ParseException,
                                                decodeFileEither)
import           Env                           (getBoolFromEnv, getIntFromEnv,
                                                getOptStringFromEnv,
                                                getStringFromEnv)
import           Network.HTTP.Types.URI        (urlEncode)
import           System.Directory
import           System.FilePath               (addExtension, combine,
                                                dropExtension,
                                                dropTrailingPathSeparator,
                                                isAbsolute, joinPath, normalise,
                                                splitPath, takeExtension)

urlEncodeString :: String -> String
urlEncodeString = unpack . urlEncode True . pack

generatePostUrlFromRelativeFile :: String -> FilePath -> String
generatePostUrlFromRelativeFile baseHost filePath = baseHost <> "/post/" <> processPostPathParts (map (T.pack . dropTrailingPathSeparator) (splitPath . dropExtension $ filePath))

processPostPathParts :: [T.Text] -> String
processPostPathParts = helper "" where
    helper :: String -> [T.Text] -> String
    helper acc []        = acc
    helper acc ["index"] = acc
    helper acc (l:ll)    = helper (acc ++ T.unpack l ++ "/") ll

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

checkCategoriesPosts :: [PostCategoryInfo] -> FilePath -> IO (Maybe ())
checkCategoriesPosts categories' p = do
  metaFiles <- listPostInfoFiles p
  checkRes <- findInvalidCategoriesPosts categories' metaFiles
  forM_ checkRes (\(f, categories) -> do
    putStrLn $ "Post " ++ show f ++ " has unknown categories: " ++ intercalate ", " categories)
  case checkRes of
    [] -> return $ Just ()
    _  -> return Nothing

findInvalidCategoriesPosts :: [PostCategoryInfo] -> [FilePath] -> IO [(FilePath, [String])]
findInvalidCategoriesPosts postCategories files = let
  categoriesNames :: [String]
  categoriesNames = map postCategoryName postCategories
  result :: [IO (FilePath, [String])]
  result = map (\f -> do
      res <- (decodeFileEither f :: IO (Either ParseException PostInfo))
      case res of
        (Left _)                                       -> return (f, [])
        (Right (PostInfo { categories = categories })) -> return (f, filter (`notElem` categoriesNames) categories)
    ) files
  in do
    unfilteredRes <- sequence result
    return $ filter (not . null . snd) unfilteredRes

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
getAppConfigFromEnv = let
    generatePageSettings :: Bool -> Bool -> PageSettings
    generatePageSettings enableIndex enableCategories = PageSettings $ case (enableIndex, enableCategories) of
      (True, True)   -> []
      (False, True)  -> [IndexPage]
      (True, False)  -> [CategoryPage]
      (False, False) -> [IndexPage, CategoryPage]
  in do
    blogDepthLimit <- getIntFromEnv "DEPTH_LEVEL" 1
    redisHost <- getStringFromEnv "REDIS_HOST" "localhost"
    redisPort <- getIntFromEnv "REDIS_PORT" 6379
    dbPath <- getStringFromEnv "DB_PATH" "./blog.db"
    enableIndexPage <- getBoolFromEnv "ENABLE_INDEX" True
    enableCategories <- getBoolFromEnv "ENABLE_CATEGORIES" True
    siteName <- getOptStringFromEnv "SITE_NAME"
    siteHost <- getOptStringFromEnv "SITE_HOST"
    robotsFilePath <- getOptStringFromEnv "ROBOTS_TXT_PATH"

    return $ AppConfig {
      postsCategories = [],
      renderSettings = defaultPostRenderOpts,
      disabledPages = generatePageSettings enableIndexPage enableCategories,
      .. }

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
