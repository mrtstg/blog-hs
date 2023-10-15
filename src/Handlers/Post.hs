{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Post
  ( getPostR
  ) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Hash.MD5 (Str(..), md5s)
import qualified Data.Text as T
import qualified Database.Redis as R
import Foundation
import Parser (parseMarkdown)
import Parser.Html (markdownToWidget)
import Parser.Types (MarkdownBlock)
import System.Directory (doesFileExist)
import System.FilePath
import System.IO (readFile')
import Yesod.Core

data CachedMarkdown
  = BString B.ByteString
  | MD [MarkdownBlock]

readFileAndCache :: R.Connection -> FilePath -> IO (Either String CachedMarkdown)
readFileAndCache conn path = do
  fileText <- readFile' path
  let parseRes = parseMarkdown fileText
  case parseRes of
    (Left e) -> return $ Left (show e)
    (Right md) -> do
      let jsonString = JSON.encode md
      let jsonString' = toStrict jsonString
      _ <- R.runRedis conn $ do R.set (B.pack $ md5s (Str path)) jsonString'
      return $ Right (BString jsonString')

getFileAndParse :: R.Connection -> FilePath -> IO (Either String CachedMarkdown)
getFileAndParse conn path = do
  let packedPath = B.pack $ md5s (Str path)
  v <- R.runRedis conn $ do R.get packedPath
  case v of
    (Left e) -> do
      print e
      readFileAndCache conn path
    (Right res) ->
      case res of
        Nothing -> do
          putStrLn "Didnt found cache!"
          readFileAndCache conn path
        (Just v') -> do
          putStrLn "Used cached!"
          let markdownBlocks = JSON.decode (fromStrict v')
          case markdownBlocks of
            (Just v'') -> return $ Right (MD v'')
            Nothing -> return $ Left "Failed to decode JSON!"

getMarkdown :: R.Connection -> FilePath -> IO (Either String CachedMarkdown)
getMarkdown conn path = do
  res <- getFileAndParse conn path
  case res of
    e@(Left _) -> return e
    md@(Right (MD _)) -> return md
    (Right (BString bs)) -> do
      let markdownBlocks = JSON.decode (fromStrict bs)
      case markdownBlocks of
        (Just v) -> return $ Right (MD v)
        Nothing -> return $ Left "Failed to decode JSON!"

getPostR :: [T.Text] -> Handler Html
getPostR pathParts = do
  App {..} <- getYesod
  case pathParts of
    [] -> notFound
    lst
      | length lst > postDepthLimit -> invalidArgs []
    _ -> do
      let finalPath =
            foldr (flip combine . T.unpack) "./templates" (init pathParts) </>
            (T.unpack (last pathParts) ++ ".md")
      fileExists <- liftIO (doesFileExist finalPath)
      if not fileExists
        then notFound
        else do
          mdRes <- liftIO $ getMarkdown redisConnectionPool finalPath
          case mdRes of
            (Left _) -> notFound
            (Right (BString _)) -> error "Unreachable pattern!"
            (Right (MD md)) -> do
              defaultLayout $ do
                setTitle $ toHtml finalPath
                [whamlet|
<section .content>
  ^{markdownToWidget md}
|]
