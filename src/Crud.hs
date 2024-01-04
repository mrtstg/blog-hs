{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Crud
  ( findPostByFilename,
  selectLatestPosts,
  isPostsAvailable
  ) where

import           Data.Text               (Text)
import           Database.Persist
import           Database.Persist.Sqlite
import           Foundation

pageSize :: Int
pageSize = 10

findPostByFilename :: Text -> String -> IO (Maybe (Entity Post))
findPostByFilename dbPath fPath =
  runSqlite dbPath $ do
    posts <- selectList [PostFile ==. fPath] [LimitTo 1]
    case posts of
      []    -> return Nothing
      (p:_) -> return $ Just p

selectLatestPosts :: Text -> Int -> IO [Entity Post]
selectLatestPosts dbPath pageNumber = runSqlite dbPath $ do selectList [] [Desc PostDate, OffsetBy (pageSize * max 0 (pageNumber - 1)), LimitTo 10]

isPostsAvailable :: Text -> Int -> IO Bool
isPostsAvailable dbPath pageNumber = runSqlite dbPath $ do
    postsCount <- count ([] :: [Filter Post])
    if postsCount - (pageSize * max 1 pageNumber) > 0 then return True else return False
