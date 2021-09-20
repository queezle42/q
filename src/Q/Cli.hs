{-# LANGUAGE ApplicativeDo #-}

module Q.Cli (main) where

import qualified Q.AlarmClock
import qualified Q.Dashboard
import qualified Q.Interface
import qualified Q.Pomodoro
import Q.Wallpaper (generateWallpaper)
import qualified Q.Hardware.BeatStep
import qualified Q.Hardware.G815
import qualified Q.System

import Control.Monad (join)
import Options.Applicative
import Prelude

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
    command "interface" (info (pure Q.Interface.main) (progDesc "Terminal UI experiments.")) <>
    command "pomodoro" (info (Q.Pomodoro.run <$> pomodoroOptionsParser) (progDesc "Control the pomodoro timer.")) <>
    command "wallpaper" (info (pure generateWallpaper) (progDesc "Generates a new wallpaper.")) <>
    command "g815" (info g815Parser (progDesc "Animate G815 keyboard leds.")) <>
    command "beatstep" (info (pure Q.Hardware.BeatStep.run) (progDesc "Parses BeatStep midi dump from aseqdump.")) <>
    command "system" (info systemParser (progDesc "TODO"))
  )

alarmClockParser :: Parser (IO ())
alarmClockParser = hsubparser (
    command "daemon" (info (pure Q.AlarmClock.runDaemon) (progDesc "Start the primary alarm clock daemon"))
  )

g815Parser :: Parser (IO ())
g815Parser = hsubparser (
    command "daemon" (info (pure Q.Hardware.G815.run) (progDesc "Start the g815 led control daemon. Output should be consumed by g810-led."))
  )

systemParser :: Parser (IO ())
systemParser = hsubparser (
    command "idle" (info (pure (Q.System.execSetIdle True)) (progDesc "Report, that the system is idle.")) <>
    command "not-idle" (info (pure (Q.System.execSetIdle False)) (progDesc "Report, that the system is no longer idle.")) <>
    command "watch-idle" (info (pure (Q.System.execWatchIdle)) (progDesc "Print the current idle value to the console.")) <>
    command "daemon" (info (pure Q.System.execSystemDaemon) (progDesc "Start the system daemon. Expects a control socket passed via socket activation."))
  )

pomodoroOptionsParser :: Parser String
pomodoroOptionsParser = strArgument (metavar "TASK" <> help "foobar")
