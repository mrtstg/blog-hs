{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handlers.Home
  ( getHomeR
  ) where

import Foundation
import Yesod.Core

getHomeR :: Handler Html
getHomeR =
  defaultLayout $ do
    setTitle "123"
    [whamlet|<p> Hello!|]
