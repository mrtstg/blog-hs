{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Crud
  ( findPostByFilename,
  selectLatestPosts,
  isPostsAvailable,
  selectAllPosts,
  createPostPhoto,
  initiatePosts,
  getPostCategories,
  findCategoryByName,
  isCategoryPostsAvailable,
  findPostsByCategory
  ) where

import           App.PostInfo               (PostInfo (..))
import           App.Utils                  (normaliseFilePath)
import           Control.Monad              (forM_)
import           Control.Monad.Trans.Reader
import qualified Data.Map                   as Map
import           Database.Persist
import           Database.Persist.Sqlite
import           Foundation
import           Yesod.Core                 (MonadUnliftIO)

pageSize :: Int
pageSize = 10

isCategoryPostsAvailable :: (MonadUnliftIO m) => Int -> CategoryId -> ReaderT SqlBackend m Bool
isCategoryPostsAvailable pageNumber cId = do
  postEntitiesCount <- count [PostCategoryCategory ==. cId]
  return $ (postEntitiesCount - (pageSize * max 1 pageNumber)) > 0

findPostsByCategory :: (MonadUnliftIO m) => Int -> CategoryId -> ReaderT SqlBackend m [Entity Post]
findPostsByCategory pageNumber cId = do
  postEntities <- selectList [PostCategoryCategory ==. cId] []
  let postIds = map (\(Entity _ (PostCategory pId _)) -> pId) postEntities
  selectList [PostId <-. postIds] [
    Desc PostDate, OffsetBy (pageSize * max 0 (pageNumber - 1)), LimitTo pageSize
    ]

findCategoryByName :: (MonadUnliftIO m) => String -> ReaderT SqlBackend m (Maybe (Entity Category))
findCategoryByName catName = do
  res <- selectList [CategoryName ==. catName] [LimitTo 1]
  case res of
    []    -> return Nothing
    (p:_) -> return $ Just p

findPostByFilename :: (PersistQueryRead SqlBackend, MonadUnliftIO m) => String -> ReaderT SqlBackend m (Maybe (Entity Post))
findPostByFilename fPath = do
  posts <- selectList [PostFile ==. fPath] [LimitTo 1]
  case posts of
    []    -> return Nothing
    (p:_) -> return $ Just p

selectLatestPosts :: (MonadUnliftIO m) => Int -> ReaderT SqlBackend m [Entity Post]
selectLatestPosts pageNumber = do selectList [] [Desc PostDate, OffsetBy (pageSize * max 0 (pageNumber - 1)), LimitTo pageSize]

selectAllPosts :: (MonadUnliftIO m) => ReaderT SqlBackend m [Entity Post]
selectAllPosts = selectList [] []

isPostsAvailable :: (MonadUnliftIO m) => Int -> ReaderT SqlBackend m Bool
isPostsAvailable pageNumber = do
    postsCount <- count ([] :: [Filter Post])
    if postsCount - (pageSize * max 1 pageNumber) > 0 then return True else return False

createPostPhoto :: (MonadUnliftIO m) => Entity Post -> String -> ReaderT SqlBackend m ()
createPostPhoto (Entity pid _) url = do
  _ <- insert $ PostPhoto url pid
  return ()

getPostCategories :: (MonadUnliftIO m) => PostId -> ReaderT SqlBackend m [Category]
getPostCategories pId = do
    relatedCategories <- selectList [PostCategoryPost ==. pId] []
    categoriesInfoMap <- getMany (map (\(Entity _ (PostCategory _ cId)) -> cId) relatedCategories)
    let categoriesInfo = Map.elems categoriesInfoMap
    return categoriesInfo

type PostsAmount = Int
initiatePosts :: (MonadUnliftIO m) => [(FilePath, PostInfo)] -> ReaderT SqlBackend m PostsAmount
initiatePosts = helper 0 where
    helper :: (MonadUnliftIO m) => Int -> [(FilePath, PostInfo)] -> ReaderT SqlBackend m PostsAmount
    helper acc [] = return acc
    helper acc ((f, PostInfo name' desc dt img categories'):ps) = do
      postId <- insert $ Post (normaliseFilePath f) name' desc dt
      case img of
        Nothing     -> return ()
        (Just img') -> mapM_ (\x -> insert $ PostPhoto x postId) img'
      forM_ categories' (\c -> do
          categoryId <- getBy $ UniqueCategory c
          case categoryId of
            Nothing    -> return ()
            (Just (Entity cId _)) -> do
                _ <- insert $ PostCategory postId cId
                return ()
          )
      helper (acc + 1) ps
