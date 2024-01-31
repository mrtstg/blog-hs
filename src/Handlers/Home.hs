{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handlers.Home
  ( getHomeR
  ) where

import           App.Config              (AppConfig (disabledPages))
import           App.Config.PageSettings (PageName (..), PageSettings (..))
import           Crud                    (isPostsAvailable, selectLatestPosts)
import           Data.Function           ((&))
import           Data.Text
import           Database.Persist        (Entity (..))
import           Foundation
import           System.FilePath         (dropExtensions)
import           Text.Read
import           Yesod.Core

getPageValue :: Maybe Text -> Int
getPageValue param = case param of
  Nothing -> 1
  (Just v) -> case (readMaybe (unpack v) :: Maybe Int) of
    (Just v') -> v'
    Nothing   -> 1

getHomeR :: Handler Html
getHomeR = do
  App { .. } <- getYesod
  let (PageSettings disabledPages') = disabledPages config
  if IndexPage `Prelude.elem` disabledPages' then notFound else do
    pageParam <- lookupGetParam "page"
    let pageValue = getPageValue pageParam
    posts <- liftIO $ selectLatestPosts dbPath pageValue
    postsAvailable <- liftIO $ isPostsAvailable dbPath pageValue
    defaultLayout $ do
        setTitle "Latest posts"
        [whamlet|
$if Prelude.null posts
    <h1> No posts!
$else
    <ul>
        $forall post <- posts
            $case post
                $of Entity _ post'
                    <li><a href="/post/#{(dropExtensions . postFile) post'}">#{postTitle post'}
$if pageValue /= 1
    <a href="/?page=#{pageValue - 1}">Previous page
$if postsAvailable
    <a href="/?page=#{pageValue + 1}">Next page
|]
