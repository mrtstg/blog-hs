{-# LANGUAGE DeriveGeneric #-}

module App.PostInfo
  ( PostInfo(..)
  , parsePostInfoFromFile
  ) where

import           Data.Yaml
import           GHC.Generics

data PostInfo = PostInfo
  { name        :: String
  , description :: String
  } deriving (Generic, Show)

parsePostInfoFromFile :: FilePath -> IO (Either ParseException PostInfo)
parsePostInfoFromFile = decodeFileEither

instance FromJSON PostInfo
