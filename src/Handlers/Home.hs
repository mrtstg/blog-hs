{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handlers.Home
  ( getHomeR
  ) where

import           App.Config       (enableIndexPage)
import           Crud             (isPostsAvailable, selectLatestPosts)
import           Data.Function    ((&))
import           Data.Text
import           Database.Persist (Entity (..))
import           Foundation
import           System.FilePath  (dropExtensions)
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
  if not (config & enableIndexPage) then notFound else do
    pageParam <- lookupGetParam "page"
    let pageValue = getPageValue pageParam
    posts <- liftIO $ selectLatestPosts dbPath pageValue
    postsAvailable <- liftIO $ isPostsAvailable dbPath pageValue
    defaultLayout $ do
        setTitle "Latest posts"
        [whamlet|
<div .container.is-centered.p-5>
    $if Prelude.null posts
        <h2 .title.is-2> No posts on this page!
        <a href="/?page=#{pageValue - 1}"><button class="button is-large"> Previous page
    $else
        <h2 .title.is-2> Latest posts
        $forall post <- posts
            $case post
                $of Entity _ post'
                    <div .box.m-3>
                        <a href="/post/#{(dropExtensions . postFile) post'}">
                            <h3 .title.is-3> #{postTitle post'}
                        <p .subtitle.is-5> #{show $ postDate post'}
                        <div .content>
                            <p> #{postDescriptinon post'}
        <div .buttons.are-large.is-fullwidth.is-centered.p-5>
            $if pageValue /= 1
                <a href="/?page=#{pageValue - 1}">
                    <button .button>Previous page
            $if postsAvailable
                <a href="/?page=#{pageValue + 1}">
                    <button .button>Next page
|]
