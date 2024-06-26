{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.PostByCategory (getCategoryR) where

import           App.Config              (AppConfig (disabledPages))
import           App.Config.PageSettings (PageName (..), PageSettings (..))
import           App.Utils               (urlEncodeString)
import           Crud
import qualified Data.Text               as T
import           Database.Persist
import           Foundation
import           System.FilePath         (dropExtensions)
import           Text.Read
import           Yesod.Core
import           Yesod.Persist           (YesodPersist (runDB))

getPageValue :: Maybe T.Text -> Int
getPageValue param = case param of
  Nothing -> 1
  (Just v) -> case (readMaybe (T.unpack v) :: Maybe Int) of
    (Just v') -> v'
    Nothing   -> 1

getCategoryR :: T.Text -> Handler Html
getCategoryR category = do
  App { .. } <- getYesod
  let (PageSettings disabledPages') = disabledPages config
  if CategoryPage `elem` disabledPages' then notFound else do
    findRes <- runDB $ findCategoryByName (T.unpack category)
    case findRes of
      Nothing -> notFound
      (Just (Entity cId category')) -> do
        pageParam <- lookupGetParam "page"
        let pageValue = getPageValue pageParam
        posts <- runDB $ findPostsByCategory pageValue cId
        postsAvailable <- runDB $ isCategoryPostsAvailable pageValue cId
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
  <a href=@{CategoryR category}?page=#{pageValue - 1}">Previous page
$if postsAvailable
  <a href=@{CategoryR category}?page=#{pageValue + 1}">Next page
|]
