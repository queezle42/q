{-# LANGUAGE ApplicativeDo #-}

module Q.Cli (main) where

import qualified Q.AlarmClock
import qualified Q.Dashboard
import qualified Q.Pomodoro
import Q.Wallpaper (generateWallpaper)
import qualified Q.G815

import Control.Monad (join)
import Options.Applicative

main :: IO ()
main = join parse

parserPrefs :: ParserPrefs
parserPrefs = prefs showHelpOnEmpty

parse :: IO (IO ())
parse = customExecParser parserPrefs parser

parser :: ParserInfo (IO ())
parser = info (mainParser <**> helper)
  (fullDesc <> header "q - queezles tools")

mainParser :: Parser (IO ())
mainParser = hsubparser (
    command "alarmclock" (info alarmClockParser (progDesc "Alarm clock subcommand.")) <>
    command "dashboard" (info (pure Q.Dashboard.run) (progDesc "Start the dashboard tui.")) <>
    command "pomodoro" (info (Q.Pomodoro.run <$> pomodoroOptionsParser) (progDesc "Control the pomodoro timer.")) <>
    command "wallpaper" (info (pure generateWallpaper) (progDesc "Generates a new wallpaper.")) <>
    command "g815" (info (pure Q.G815.run) (progDesc "Animate G815 keyboard leds. For consumption by g810-led."))
  )

alarmClockParser :: Parser (IO ())
alarmClockParser = hsubparser (
    command "daemon" (info (pure Q.AlarmClock.runDaemon) (progDesc "Start the primary alarm clock daemon"))
  )

pomodoroOptionsParser :: Parser String
pomodoroOptionsParser = strArgument (metavar "TASK" <> help "foobar")
