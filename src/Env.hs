module Env
  ( tryIntFromEnv
  , getIntFromEnv
  , getStringFromEnv
  ) where

import           System.Environment (lookupEnv)
import           Text.Read          (readMaybe)

type DefaultInt = Int

type EnvVarName = String

type DefaultString = String

tryIntFromEnv :: EnvVarName -> IO (Maybe Int)
tryIntFromEnv varName = do
  v <- lookupEnv varName
  case v of
    Nothing -> return Nothing
    (Just v') -> do
      let parseRes = (readMaybe v' :: Maybe Int)
      case parseRes of
        Nothing -> do
          return Nothing
        (Just v'') -> return $ Just v''

getIntFromEnv :: EnvVarName -> DefaultInt -> IO Int
getIntFromEnv varName defaultValue = do
  res <- tryIntFromEnv varName
  return $
    case res of
      Nothing  -> defaultValue
      (Just v) -> v

getStringFromEnv :: EnvVarName -> DefaultString -> IO String
getStringFromEnv varName defaultValue = do
  v <- lookupEnv varName
  case v of
    Nothing   -> return defaultValue
    (Just v') -> return v'
