{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Crud
  ( findPostByFilename
  ) where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Foundation

findPostByFilename :: Text -> String -> IO (Maybe (Entity Post))
findPostByFilename dbPath fPath =
  runSqlite dbPath $ do
    posts <- selectList [PostFile ==. fPath] [LimitTo 1]
    case posts of
      [] -> return Nothing
      (p:_) -> return $ Just p
