{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Robots (getRobotsR) where

import           App.Config              (AppConfig (disabledPages, robotsFilePath))
import           App.Config.PageSettings
import           Foundation
import           Yesod.Core

getRobotsR :: Handler ()
getRobotsR = do
    App { .. } <- getYesod
    let (PageSettings disabledPages') = disabledPages config
    if RobotsPage `elem` disabledPages' then notFound else do
      let robotsPath = robotsFilePath config
      case robotsPath of
        Nothing  -> notFound
        (Just v) -> sendFile "text/plain" v
