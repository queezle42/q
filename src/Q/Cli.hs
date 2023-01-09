{-# LANGUAGE ApplicativeDo #-}

module Q.Cli (main) where

import Q.AlarmClock qualified
import Q.Dashboard qualified
import Q.Home
import Q.Pomodoro qualified
import Q.Wallpaper (generateWallpaper)
import Q.Hardware.BeatStep qualified
import Q.Hardware.G815 qualified
import Q.Pinephone qualified
import Q.System qualified
import Q.VT qualified

import Control.Monad (join)
import Options.Applicative
import Prelude
import Quasar

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
mainParser = hsubparser $ mconcat [
    command "alarmclock" (info alarmClockParser (progDesc "Alarm clock subcommand.")),
    command "dashboard" (info (pure Q.Dashboard.run) (progDesc "Start the dashboard tui.")),
    command "pomodoro" (info (Q.Pomodoro.run <$> pomodoroOptionsParser) (progDesc "Control the pomodoro timer.")),
    command "wallpaper" (info (pure generateWallpaper) (progDesc "Generates a new wallpaper.")),
    command "g815" (info (quasarApp g815Parser) (progDesc "Animate G815 keyboard leds.")),
    command "beatstep" (info (pure Q.Hardware.BeatStep.run) (progDesc "Parses BeatStep midi dump from aseqdump.")),
    command "vt" (info vtParser (progDesc "VT experiments.")),
    command "system" (info (quasarApp systemParser) (progDesc "System management subcommands")),
    command "pinephone" (info (pure Q.Pinephone.run) (progDesc "Pinephone manager daemon")),
    command "home" (info (quasarApp homeParser) (progDesc "Home automation subcommands"))
  ]

quasarApp :: Parser (QuasarIO ()) -> Parser (IO ())
quasarApp = fmap (runQuasarAndExit (stderrLogger LogLevelWarning))

alarmClockParser :: Parser (IO ())
alarmClockParser = hsubparser (
    command "daemon" (info (pure Q.AlarmClock.runDaemon) (progDesc "Start the primary alarm clock daemon"))
  )

g815Parser :: Parser (QuasarIO ())
g815Parser = hsubparser (
    command "daemon" (info (pure Q.Hardware.G815.run) (progDesc "Start the g815 led control daemon. Output should be consumed by g810-led."))
  )

systemParser :: Parser (QuasarIO ())
systemParser = hsubparser (
    command "idle" (info (pure (Q.System.execSetIdle True)) (progDesc "Report, that the system is idle.")) <>
    command "not-idle" (info (pure (Q.System.execSetIdle False)) (progDesc "Report, that the system is no longer idle.")) <>
    command "watch-idle" (info (pure Q.System.execWatchIdle) (progDesc "Print the current idle value to the console.")) <>
    command "daemon" (info (pure Q.System.execSystemDaemon) (progDesc "Start the system daemon. Expects a control socket passed via socket activation."))
  )

vtParser :: Parser (IO ())
vtParser = hsubparser $ mconcat [
  command "lockswitch" (info (pure Q.VT.vtLockSwitch) (progDesc "Disallow VT switching.")),
  command "unlockswitch" (info (pure Q.VT.vtUnlockSwitch) (progDesc "Allow VT switching.")),
  command "probe" (info (pure Q.VT.runProbes) (progDesc "Probe potential VT devices."))
  ]

homeParser :: Parser (QuasarIO ())
homeParser = hsubparser $ mconcat [
  command "daemon" (info (homeDaemon <$> homeDaemonOptionsParser) (progDesc "Run home automation daemon."))
  ]

homeDaemonOptionsParser :: Parser String
homeDaemonOptionsParser = strArgument (metavar "MQTT" <> help "MQTT server URI.")

pomodoroOptionsParser :: Parser String
pomodoroOptionsParser = strArgument (metavar "TASK" <> help "foobar")
