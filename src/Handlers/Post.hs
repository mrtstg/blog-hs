{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handlers.Post
  ( getPostR
  ) where

import           App.Config              (siteHost, siteName)
import           App.PostInfo            (PostInfo (..), parsePostInfoFromFile)
import           App.Redis               (cacheRedisDataMD5,
                                          getCachedRedisDataMD5)
import           App.Utils               (processPostPathParts)
import           Control.Concurrent      (threadDelay)
import qualified Control.Concurrent.Lock as Lock
import qualified Data.Aeson              as JSON
import qualified Data.ByteString.Char8   as B
import           Data.ByteString.Lazy    (fromStrict, toStrict)
import qualified Data.List               as L
import qualified Data.Text               as T
import qualified Database.Redis          as R
import           Foundation
import           Parser                  (parseMarkdown)
import           Parser.Html             (markdownToWidget)
import           Parser.Types            (MarkdownBlock)
import           System.Directory        (doesFileExist)
import           System.FilePath
import           System.IO               (readFile')
import           Text.Blaze.Html         (preEscapedToHtml)
import           Yesod.Core

data CachedMarkdown
  = CachedMarkdownString B.ByteString
  | MD [MarkdownBlock]

data CachedPostInfo = CachedPostInfoString B.ByteString | PInfo PostInfo

readPostInfoFileAndCache :: R.Connection -> FilePath -> IO (Either String CachedPostInfo)
readPostInfoFileAndCache conn path = do
    postParseRes <- parsePostInfoFromFile path
    case postParseRes of
      (Left e) -> return $ Left (show e)
      (Right postInfo) -> do
        let jsonString = toStrict $ JSON.encode postInfo
        cacheRedisDataMD5 conn path jsonString
        return $ Right (CachedPostInfoString jsonString)

readMarkdownFileAndCache :: R.Connection -> FilePath -> IO (Either String CachedMarkdown)
readMarkdownFileAndCache conn path = do
  fileText <- readFile' path
  let parseRes = parseMarkdown fileText
  case parseRes of
    (Left e) -> return $ Left (show e)
    (Right md) -> do
      let jsonString = toStrict $ JSON.encode md
      cacheRedisDataMD5 conn path jsonString
      return $ Right (CachedMarkdownString jsonString)

getMarkdownFileAndParse :: Lock.Lock -> R.Connection -> FilePath -> IO (Either String CachedMarkdown)
getMarkdownFileAndParse lock conn path =
  let innerCache :: FilePath -> IO (Either String CachedMarkdown)
      innerCache p' = do
        locked' <- Lock.locked lock
        if locked'
          then do
            _ <- threadDelay 50000
            getMarkdownFileAndParse lock conn p'
          else do
            Lock.acquire lock
            res <- readMarkdownFileAndCache conn p'
            Lock.release lock
            return res
   in do v <- getCachedRedisDataMD5 conn path
         case v of
           Nothing -> innerCache path
           (Just v') -> do
             let markdownBlocks = JSON.decode (fromStrict v')
             case markdownBlocks of
               (Just v'') -> return $ Right (MD v'')
               Nothing    -> return $ Left "Failed to decode JSON!"

getPostInfoFileAndParse :: Lock.Lock -> R.Connection -> FilePath -> IO (Either String CachedPostInfo)
getPostInfoFileAndParse lock conn path = let
    innerCache :: FilePath -> IO (Either String CachedPostInfo)
    innerCache p' = do
        locked' <- Lock.locked lock
        if locked' then do
            _ <- threadDelay 50000
            getPostInfoFileAndParse lock conn p'
        else do
            Lock.acquire lock
            res <- readPostInfoFileAndCache conn p'
            Lock.release lock
            return res
    in do v <- getCachedRedisDataMD5 conn path
          case v of
            Nothing   -> innerCache path
            (Just v') -> do
                let postInfo = JSON.decode (fromStrict v')
                case postInfo of
                  (Just v'') -> return $ Right (PInfo v'')
                  Nothing    -> return $ Left "Failed to decode JSON!"

getMarkdown :: Lock.Lock -> R.Connection -> FilePath -> IO (Either String CachedMarkdown)
getMarkdown lock conn path = do
  res <- getMarkdownFileAndParse lock conn path
  case res of
    e@(Left _) -> return e
    md@(Right (MD _)) -> return md
    (Right (CachedMarkdownString bs)) -> do
      let markdownBlocks = JSON.decode (fromStrict bs)
      case markdownBlocks of
        (Just v) -> return $ Right (MD v)
        Nothing  -> return $ Left "Failed to decode JSON!"

getPostInfo :: Lock.Lock -> R.Connection -> FilePath -> IO (Either String CachedPostInfo)
getPostInfo lock conn path = do
    res <- getPostInfoFileAndParse lock conn path
    case res of
      e@(Left _)           -> return e
      pi'@(Right (PInfo _)) -> return pi'
      (Right (CachedPostInfoString bs)) -> do
          let res' = JSON.decode (fromStrict bs)
          case res' of
            (Just v) -> return $ Right (PInfo v)
            Nothing  -> return $ Left "Failed to decode JSON!"

getPostR :: [T.Text] -> Handler Html
getPostR pathParts = do
  App {..} <- getYesod
  case pathParts of
    [] -> notFound
    lst
      | length lst > postDepthLimit -> permissionDenied "Post depth limit overflowed"
      | otherwise -> do
      let filePath =
            foldr (flip combine . T.unpack) "./templates" (init pathParts) </>
            (T.unpack (last pathParts) ++ ".md")
      let indexFilePath = foldr (flip combine . T.unpack) "./templates" pathParts </> "index.md"
      fileExists <- liftIO (doesFileExist filePath)
      indexFileExists <- liftIO (doesFileExist indexFilePath)
      if not fileExists && not indexFileExists
        then notFound
        else do
          let filePath' =
                if fileExists
                  then filePath
                  else indexFilePath
          mdRes <- liftIO $ getMarkdown redisWriteLock redisConnectionPool filePath'
          let metaPath' = flip addExtension "yml" $ dropExtensions filePath'
          postInfoRes <- liftIO $ getPostInfo redisWriteLock redisConnectionPool metaPath'
          let siteHost' = siteHost config
          let siteName' = siteName config
          case (mdRes, postInfoRes) of
            (Right (MD md), Right (PInfo PostInfo { name = postName, description = postDescription, date = postDate, images = postImages })) -> do
              defaultLayout $ do
                toWidgetHead [hamlet|
<meta property=og:title content=#{postName}>
<meta property=og:type content=article>
<meta property=og:description content=#{postDescription}>
$maybe postImages' <- postImages
    $if not $ null postImages'
        <meta property=og:image content=#{head postImages'}>

$maybe siteHost'' <- siteHost'
    <meta property=og:url content=#{siteHost''}/post/#{processPostPathParts pathParts}>
    <script type=application/ld+json>
        {
            "@context": "https://schema.org",
            "@id": "#{siteHost''}/post/#{processPostPathParts pathParts}",
            "@type": "Blog",
            $maybe siteName'' <- siteName'
                "name": "#{siteName''}",
            "blogPost": {
                "@id": "#{siteHost''}/post/#{processPostPathParts pathParts}#BlogPosting",
                "@type": "BlogPosting",
                "name": "#{postName}",
                "headline": "#{postName}",
                "url": "#{siteHost''}/post/#{processPostPathParts pathParts}",
                "datePublished": "#{show postDate}T00:00:00",
                $maybe postImages' <- postImages
                    $if null postImages'
                        "image": [],
                    $else
                        "image": ["#{preEscapedToHtml $ L.intercalate "\", \"" postImages'}"],
                "author": {
                    "@type": "Person",
                    "name": "#TODO"
                }
            }
        }
|]
                setTitle $ toHtml postName
                [whamlet|
<div .container>
    <div .content.p-5>
        ^{markdownToWidget md}
|]
            (_, _) -> notFound
