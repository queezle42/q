module Q.Pinephone (
  run,
) where

import DBus
import DBus.Client
import Quasar
import Quasar.Prelude

run :: IO ()
run = do
  dbus <- connectSystem
  void $ addMatch dbus prepareForSleepMatch prepareForSleepHandler
  sleepForever

prepareForSleepMatch :: MatchRule
prepareForSleepMatch =
  matchAny {
    matchMember = Just "PrepareForSleep",
    matchInterface = Just "org.freedesktop.login1.Manager",
    matchPath = Just "/org/freedesktop/login1"
  }

prepareForSleepHandler :: Signal -> IO ()
prepareForSleepHandler signal =
  case signalBody signal of
    [fromVariant @Bool -> Just b] -> traceIO $ "PrepareForSleep: " <> show b
    args -> traceIO $ "Invalid arguments for PrepareForSleep: " <> show args
