{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Robots (getRobotsR) where

import           App.Config (AppConfig (robotsFilePath))
import           Foundation
import           Yesod.Core

getRobotsR :: Handler ()
getRobotsR = do
    App { .. } <- getYesod
    let robotsPath = robotsFilePath config
    case robotsPath of
      Nothing  -> notFound
      (Just v) -> sendFile "text/plain" v
