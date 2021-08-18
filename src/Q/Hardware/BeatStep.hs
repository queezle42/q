module Q.Hardware.BeatStep where

import Control.Monad (forever)
import Data.Text (Text)
import Prelude
import System.IO (stderr, getLine, hPutStrLn)

run :: IO ()
run = do
  forever $ do
    line <- getLine
    case line of
      "Waiting for data. Press Ctrl+C to end." -> return ()
      "Source  Event                  Ch  Data" -> return ()
      _ -> hPutStrLn stderr "Failed to parse line:"
    hPutStrLn stderr line
