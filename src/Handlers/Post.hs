{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Post
  ( getPostR
  ) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Lock as Lock
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
      _ <-
        R.runRedis conn $ do
          R.setOpts (B.pack $ md5s (Str path)) jsonString' (R.SetOpts (Just 60) Nothing Nothing)
      return $ Right (BString jsonString')

getFileAndParse :: Lock.Lock -> R.Connection -> FilePath -> IO (Either String CachedMarkdown)
getFileAndParse lock conn path =
  let innerCache :: FilePath -> IO (Either String CachedMarkdown)
      innerCache p' = do
        locked' <- Lock.locked lock
        if locked'
          then do
            _ <- threadDelay 50000
            getFileAndParse lock conn p'
          else do
            Lock.acquire lock
            res <- readFileAndCache conn p'
            Lock.release lock
            return res
   in do let packedPath = B.pack $ md5s (Str path)
         v <- R.runRedis conn $ do R.get packedPath
         case v of
           (Left e) -> do
             print e
             innerCache path
           (Right res) ->
             case res of
               Nothing -> do
                 putStrLn "Didnt found cache!"
                 innerCache path
               (Just v') -> do
                 putStrLn "Used cached!"
                 let markdownBlocks = JSON.decode (fromStrict v')
                 case markdownBlocks of
                   (Just v'') -> return $ Right (MD v'')
                   Nothing -> return $ Left "Failed to decode JSON!"

getMarkdown :: Lock.Lock -> R.Connection -> FilePath -> IO (Either String CachedMarkdown)
getMarkdown lock conn path = do
  res <- getFileAndParse lock conn path
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
          case mdRes of
            (Left _) -> notFound
            (Right (BString _)) -> error "Unreachable pattern!"
            (Right (MD md)) -> do
              defaultLayout $ do
                setTitle $ toHtml filePath'
                [whamlet|
<section .content>
  ^{markdownToWidget md}
|]
