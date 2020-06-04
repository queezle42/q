{-# LANGUAGE ApplicativeDo #-}

module Q.Cli (main) where

import qualified Q.Dashboard
import qualified Q.Pomodoro
import Q.Wallpaper (generateWallpaper)

import Control.Monad (join)
import Options.Applicative

mainParser :: Parser (IO ())
mainParser = hsubparser
  (
    command "dashboard" (info (pure Q.Dashboard.run) (progDesc "Start the dashboard tui.")) <>
    command "pomodoro" (info (Q.Pomodoro.run <$> pomodoroOptionsParser) (progDesc "Control the pomodoro timer.")) <>
    command "wallpaper" (info (pure generateWallpaper) (progDesc "Generates a new wallpaper."))
  )

parser :: ParserInfo (IO ())
parser = info (mainParser <**> helper)
  (fullDesc <> header "q - queezles tools")

parserPrefs :: ParserPrefs
parserPrefs = prefs showHelpOnEmpty

parse :: IO (IO ())
parse = customExecParser parserPrefs parser

main :: IO ()
main = join parse

pomodoroOptionsParser :: Parser String
pomodoroOptionsParser = strArgument (metavar "TASK" <> help "foobar")
