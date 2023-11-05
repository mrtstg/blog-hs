module Main
  ( main
  ) where

import App.Commands
import App.Config (AppConfig(..))
import App.Config
import App.Parser (appParser)
import App.Types (AppCommand(..))
import App.Types
import App.Utils
import Data.Function ((&))
import Foundation
import Options.Applicative
import System.Exit

main :: IO ()
main = do
  opts <-
    execParser (info (appParser <**> helper) (fullDesc <> progDesc "Lightweight blog on Haskell!"))
  configRes <- getAppConfig (opts & configPath)
  case configRes of
    (Left err) -> do
      print err
      exitWith (ExitFailure 2)
    (Right cfg) -> runCommand (opts & appCommand) cfg
