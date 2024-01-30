{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.PostByCategory (getCategoryR) where

import           Crud
import qualified Data.Text        as T
import           Database.Persist
import           Foundation
import           Text.Blaze.Html  (toHtml)
import           Yesod.Core

getCategoryR :: T.Text -> Handler Html
getCategoryR category = do
  App { .. } <- getYesod
  findRes <- liftIO $ findCategoryByName dbPath (T.unpack category)
  case findRes of
    Nothing -> notFound
    (Just (Entity _ category')) ->
      defaultLayout $ do
        setTitle $ toHtml (categoryDisplayName category')
        [whamlet|
<h1> #{categoryDisplayName category'}
|]
