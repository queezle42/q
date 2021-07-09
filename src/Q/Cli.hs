{-# LANGUAGE ApplicativeDo #-}

module Q.Cli (main) where

import qualified Q.AlarmClock
import qualified Q.Dashboard
import qualified Q.Pomodoro
import Q.Wallpaper (generateWallpaper)
import qualified Q.Hardware.BeatStep
import qualified Q.Hardware.G815

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
    command "g815" (info g815Parser (progDesc "Animate G815 keyboard leds.")) <>
    command "beatstep" (info (pure Q.Hardware.BeatStep.run) (progDesc "Parses BeatStep midi dump from aseqdump."))
  )

alarmClockParser :: Parser (IO ())
alarmClockParser = hsubparser (
    command "daemon" (info (pure Q.AlarmClock.runDaemon) (progDesc "Start the primary alarm clock daemon"))
  )

g815Parser :: Parser (IO ())
g815Parser = hsubparser (
    command "idle" (info (pure (Q.Hardware.G815.runSetIdle True)) (progDesc "Report, that the system is idle.")) <>
    command "not-idle" (info (pure (Q.Hardware.G815.runSetIdle False)) (progDesc "Report, that the system is no longer idle.")) <>
    command "daemon" (info (pure Q.Hardware.G815.run) (progDesc "Start the g815 led control daemon. Output should be consumed by g810-led."))
  )

pomodoroOptionsParser :: Parser String
pomodoroOptionsParser = strArgument (metavar "TASK" <> help "foobar")
