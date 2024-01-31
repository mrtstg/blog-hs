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

import           App.PostInfo            (PostInfo (..))
import           App.Utils               (normaliseFilePath)
import           Control.Monad           (forM_)
import qualified Data.Map                as Map
import           Data.Text               (Text)
import           Database.Persist
import           Database.Persist.Sqlite
import           Foundation
import           Yesod.Core              (MonadUnliftIO)

type DbPath = Text

pageSize :: Int
pageSize = 10

isCategoryPostsAvailable :: DbPath -> Int -> CategoryId -> IO Bool
isCategoryPostsAvailable dbPath pageNumber cId = runSqlite dbPath $ do
  postEntitiesCount <- count [PostCategoryCategory ==. cId]
  return $ (postEntitiesCount - (pageSize * max 1 pageNumber)) > 0

findPostsByCategory :: DbPath -> Int -> CategoryId -> IO [Entity Post]
findPostsByCategory dbPath pageNumber cId = runSqlite dbPath $ do
  postEntities <- selectList [PostCategoryCategory ==. cId] []
  let postIds = map (\(Entity _ (PostCategory pId _)) -> pId) postEntities
  selectList [PostId <-. postIds] [
    Desc PostDate, OffsetBy (pageSize * max 0 (pageNumber - 1)), LimitTo pageSize
    ]

findCategoryByName :: DbPath -> String -> IO (Maybe (Entity Category))
findCategoryByName dbPath catName = runSqlite dbPath $ do
  res <- selectList [CategoryName ==. catName] [LimitTo 1]
  case res of
    []    -> return Nothing
    (p:_) -> return $ Just p

findPostByFilename :: DbPath -> String -> IO (Maybe (Entity Post))
findPostByFilename dbPath fPath =
  runSqlite dbPath $ do
    posts <- selectList [PostFile ==. fPath] [LimitTo 1]
    case posts of
      []    -> return Nothing
      (p:_) -> return $ Just p

selectLatestPosts :: DbPath -> Int -> IO [Entity Post]
selectLatestPosts dbPath pageNumber = runSqlite dbPath $ do selectList [] [Desc PostDate, OffsetBy (pageSize * max 0 (pageNumber - 1)), LimitTo pageSize]

selectAllPosts :: DbPath -> IO [Entity Post]
selectAllPosts dbPath = runSqlite dbPath $ do selectList [] []

isPostsAvailable :: DbPath -> Int -> IO Bool
isPostsAvailable dbPath pageNumber = runSqlite dbPath $ do
    postsCount <- count ([] :: [Filter Post])
    if postsCount - (pageSize * max 1 pageNumber) > 0 then return True else return False

createPostPhoto :: DbPath -> Entity Post -> String -> IO ()
createPostPhoto dbPath (Entity pid _) url = do
    runSqlite dbPath $ do
        _ <- insert $ PostPhoto url pid
        return ()

getPostCategories :: DbPath -> PostId -> IO [Category]
getPostCategories dbPath pId = runSqlite dbPath $ do
    relatedCategories <- selectList [PostCategoryPost ==. pId] []
    categoriesInfoMap <- getMany (map (\(Entity _ (PostCategory _ cId)) -> cId) relatedCategories)
    let categoriesInfo = Map.elems categoriesInfoMap
    return categoriesInfo

type PostsAmount = Int
initiatePosts :: DbPath -> [(FilePath, PostInfo)] -> IO PostsAmount
initiatePosts dbPath = helper 0 where
    helper :: Int -> [(FilePath, PostInfo)] -> IO PostsAmount
    helper acc [] = return acc
    helper acc ((f, PostInfo name' desc dt img categories'):ps) = do
        runSqlite dbPath $ do
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
