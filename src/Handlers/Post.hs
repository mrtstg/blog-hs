{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handlers.Post
  ( getPostR
  ) where

import           App.Config                    (AppConfig (disabledPages, renderSettings),
                                                redisCacheTime, siteHost,
                                                siteName)
import           App.Config.PageSettings       (PageName (..),
                                                PageSettings (..))
import           App.Config.PostRenderSettings (PostRenderSettings (..))
import           App.PostInfo                  (PostInfo (..),
                                                parsePostInfoFromFile')
import           App.Redis                     (ParseableCachedData (..),
                                                getLockCachedParseableData)
import           App.Utils                     (normaliseFilePath,
                                                processPostPathParts,
                                                urlEncodeString)
import qualified Control.Concurrent.Lock       as Lock
import           Control.Monad                 (when)
import           Crud                          (findPostByFilename,
                                                getPostCategories)
import qualified Data.Aeson                    as JSON
import           Data.ByteString.Lazy          (fromStrict)
import qualified Data.List                     as L
import qualified Data.Text                     as T
import           Database.Persist
import qualified Database.Redis                as R
import           Foundation
import           Parser                        (parseMarkdown)
import           Parser.Html                   (markdownToWidget)
import           Parser.Types                  (MarkdownBlock)
import           System.Directory              (doesFileExist)
import           System.FilePath
import           System.IO                     (readFile')
import           Text.Blaze.Html               (preEscapedToHtml)
import           Yesod.Core
import           Yesod.Persist                 (YesodPersist (runDB))

getMarkdownFileAndParse :: Lock.Lock -> R.Connection -> Int -> FilePath -> IO (Either String (ParseableCachedData [MarkdownBlock]))
getMarkdownFileAndParse lock conn timeout path = getLockCachedParseableData lock conn timeout path f where
  f = do
    fileText <- readFile' path
    return $ parseMarkdown fileText

getPostInfoFileAndParse :: Lock.Lock -> R.Connection -> Int -> FilePath -> IO (Either String (ParseableCachedData PostInfo))
getPostInfoFileAndParse lock conn timeout path = getLockCachedParseableData lock conn timeout path (parsePostInfoFromFile' path)

getMarkdown :: Lock.Lock -> R.Connection -> Int -> FilePath -> IO (Either String (ParseableCachedData [MarkdownBlock]))
getMarkdown lock conn timeout path = do
  res <- getMarkdownFileAndParse lock conn timeout path
  case res of
    e@(Left _) -> return e
    md@(Right (ParsedData _)) -> return md
    (Right (RawData bs)) -> do
      let markdownBlocks = JSON.decode (fromStrict bs)
      case markdownBlocks of
        (Just v) -> return $ Right (ParsedData v)
        Nothing  -> return $ Left "Failed to decode JSON!"

getPostInfo :: Lock.Lock -> R.Connection -> Int -> FilePath -> IO (Either String (ParseableCachedData PostInfo))
getPostInfo lock conn timeout path = do
    res <- getPostInfoFileAndParse lock conn timeout path
    case res of
      e@(Left _)           -> return e
      pi'@(Right (ParsedData _)) -> return pi'
      (Right (RawData bs)) -> do
          let res' = JSON.decode (fromStrict bs)
          case res' of
            (Just v) -> return $ Right (ParsedData v)
            Nothing  -> return $ Left "Failed to decode JSON!"

createCategoryWidget :: Maybe String -> PageSettings -> Category -> WidgetFor App ()
createCategoryWidget baseHost (PageSettings disabledPages') (Category { categoryName = name, categoryDisplayName = dname })= do
  case (baseHost, CategoryPage `elem` disabledPages') of
    (Just v, False) -> do
      toWidget [hamlet|<a href=#{v}/category/#{urlEncodeString name}> #{dname}|]
    _anyOther -> do
      toWidget [hamlet|#{name} |]

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
          let redisCacheTime' = redisCacheTime config
          mdRes <- liftIO $ getMarkdown redisWriteLock redisConnectionPool redisCacheTime' filePath'
          let metaPath' = flip addExtension "yml" $ dropExtensions filePath'
          postInfoRes <- liftIO $ getPostInfo redisWriteLock redisConnectionPool redisCacheTime' metaPath'
          dbPost <- runDB $ findPostByFilename (normaliseFilePath filePath')
          case dbPost of
            Nothing -> notFound
            (Just (Entity pId _)) -> do
              let PostRenderSettings { .. } = renderSettings config
              postCategories <- runDB $ getPostCategories pId
              let siteHost' = siteHost config
              let siteName' = siteName config
              let categoryWidgets = map (createCategoryWidget siteHost' (disabledPages config)) postCategories
              case (mdRes, postInfoRes) of
                (Right (ParsedData md), Right (ParsedData (PostInfo { name = postName, description = postDescription, date = postDate, images = postImages }))) -> do
                  defaultLayout $ do
                    [whamlet|
$if postRenderTitle
  <h1> #{postName}
$if postRenderDate
  <i> #{show postDate}
$if postRenderCategories && (not . null) categoryWidgets
  <p> Categories:
    $forall widget <- categoryWidgets
      ^{widget}
<section .content>

  ^{markdownToWidget md}
                    |]
                    setTitle $ toHtml postName
                    when postRenderMeta $ toWidgetHead [hamlet|
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
