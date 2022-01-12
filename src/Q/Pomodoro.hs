module Q.Pomodoro where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Prelude
import System.IO (Handle, hFlush, hPutStrLn)
import System.Process.Typed

-- TODO:
-- User confirmation that task is completed (press buzzer? something as satisfying as putting a checkmark on paper)
-- Lock pc during break?
-- Signal when break ends (even if screen is off, so audio or a light)
-- Split into client/server (there is no need to have an open terminal)
-- Deploy gong file with nix (-> static file deployment, also required for wallpaper deployment)
-- Track completed tasks and aborted pomodoros

tomatoEmoji :: String
tomatoEmoji = "🍅︎ "

run :: String -> IO ()
run task = do
  withQBarBlock $ \qbarInputHandle -> do
    forM_ [25,24 .. 1] $ \ (minutesRemaining :: Int) -> do
      hPutStrLn qbarInputHandle $ "<active>" <> tomatoEmoji <> task <> "</active> (" <> show minutesRemaining <> "m)"
      hFlush qbarInputHandle
      threadDelay (60 * 1000000)
  playGong
  withQBarBlock $ \qbarInputHandle -> do
    forM_ [5,4 .. 1] $ \ (minutesRemaining :: Int) -> do
      hPutStrLn qbarInputHandle $ tomatoEmoji <> "break (" <> show minutesRemaining <> "m)"
      hFlush qbarInputHandle
      threadDelay (60 * 1000000)

playGong :: IO ()
playGong = runProcess_ $ setStderr nullStream $ setStdout nullStream $ shell "mpv --no-terminal --no-config --no-video ~/sounds/gong.mp3"

withQBarBlock :: (Handle -> IO ()) -> IO ()
withQBarBlock action = do
  let qbarConfig = setStdin createPipe $ shell "qbar pipe"
  withProcessTerm qbarConfig $ \ p -> do
    action $ getStdin p
