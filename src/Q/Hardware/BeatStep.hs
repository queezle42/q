module Q.Hardware.BeatStep where

import Control.Monad (forever)
import Data.Text (Text)
import System.IO (stderr, getLine, hPutStrLn)
import Qd
import Qd.Interface
import Qd.QdProtocol.Client

run :: IO ()
run = withConnectTCP $ \qdInterface -> do
  alarmTime <- runActorSetup' qdInterface [] setup
  forever $ do
    line <- getLine
    case line of
      "Waiting for data. Press Ctrl+C to end." -> return ()
      "Source  Event                  Ch  Data" -> return ()
      _ -> hPutStrLn stderr "Failed to parse line:"
    hPutStrLn stderr line
  where
    setup :: ActorSetup (PropertyProxy Text)
    setup = createProperty "alarmTime"
