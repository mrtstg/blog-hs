{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module App.PostInfo
  ( PostInfo(..)
  , parsePostInfoFromFile
  , parsePostInfoFromFile'
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
  , categories  :: ![String]
  } deriving (Generic, Show, Eq)

parsePostInfoFromFile :: FilePath -> IO (Either ParseException PostInfo)
parsePostInfoFromFile = decodeFileEither

parsePostInfoFromFile' :: FilePath -> IO (Either String PostInfo)
parsePostInfoFromFile' p = do
  res <- decodeFileEither p
  case res of
    (Left e)  -> return (Left $ show e)
    (Right r) -> return $ Right r

instance FromJSON PostInfo where
    parseJSON = withObject "PostInfo" $ \v -> PostInfo
        <$> v .: "name"
        <*> v .:? "description" .!= ""
        <*> v .: "date"
        <*> v .:? "images"
        <*> v .:? "categories" .!= []

instance JSON.ToJSON PostInfo
