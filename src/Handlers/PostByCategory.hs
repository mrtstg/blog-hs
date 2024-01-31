{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.PostByCategory (getCategoryR) where

import           App.Utils        (urlEncodeString)
import           Crud
import qualified Data.Text        as T
import           Database.Persist
import           Foundation
import           System.FilePath  (dropExtensions)
import           Text.Read
import           Yesod.Core

getPageValue :: Maybe T.Text -> Int
getPageValue param = case param of
  Nothing -> 1
  (Just v) -> case (readMaybe (T.unpack v) :: Maybe Int) of
    (Just v') -> v'
    Nothing   -> 1

getCategoryR :: T.Text -> Handler Html
getCategoryR category = do
  let encodedCategory = urlEncodeString (T.unpack category)
  App { .. } <- getYesod
  findRes <- liftIO $ findCategoryByName dbPath (T.unpack category)
  case findRes of
    Nothing -> notFound
    (Just (Entity cId category')) -> do
      pageParam <- lookupGetParam "page"
      let pageValue = getPageValue pageParam
      posts <- liftIO $ findPostsByCategory dbPath pageValue cId
      postsAvailable <- liftIO $ isCategoryPostsAvailable dbPath pageValue cId
      defaultLayout $ do
        setTitle $ toHtml (categoryDisplayName category')
        [whamlet|
<h1> #{categoryDisplayName category'}
<p> #{categoryDescription category'}
$if Prelude.null posts
  <h1> No posts!
$else
  <ul>
    $forall post <- posts
      $case post
        $of Entity _ post'
          <li><a href="/post/#{(dropExtensions . postFile) post'}">#{postTitle post'}
$if pageValue /= 1
  <a href="/category/#{urlEncodeString encodedCategory}?page=#{pageValue - 1}">Previous page
$if postsAvailable
  <a href="/category/#{urlEncodeString encodedCategory}?page=#{pageValue + 1}">Next page
|]
