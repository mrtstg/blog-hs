{-# LANGUAGE DeriveGeneric #-}

module App.PostInfo
  ( PostInfo(..)
  , parsePostInfoFromFile
  ) where

import qualified Data.Aeson         as JSON
import           Data.Time.Calendar (Day)
import           Data.Yaml
import           GHC.Generics

data PostInfo = PostInfo
  { name        :: !String
  , description :: !String
  , date        :: !Day
  , images      :: !(Maybe [String])
  } deriving (Generic, Show, Eq)

parsePostInfoFromFile :: FilePath -> IO (Either ParseException PostInfo)
parsePostInfoFromFile = decodeFileEither

instance FromJSON PostInfo

instance JSON.ToJSON PostInfo
