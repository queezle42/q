module Q.G815 where

import Control.Concurrent (threadDelay)
--import Control.Monad (forever)
import System.IO (stdout, hFlush, hPutStrLn)

run :: IO ()
run = do
  do
    hPutStrLn stdout $ "a ff0000"
    hPutStrLn stdout $ "k logo 000000"
    hPutStrLn stdout $ "k h 0000ff"
    hPutStrLn stdout $ "k j 0000ff"
    hPutStrLn stdout $ "k k 0000ff"
    hPutStrLn stdout $ "k l 0000ff"
    --hPutStrLn stdout $ "k G1 ff0050"
    --hPutStrLn stdout $ "k G2 ff0050"
    --hPutStrLn stdout $ "k G3 ff0050"
    --hPutStrLn stdout $ "k G4 ff0050"
    --hPutStrLn stdout $ "k G5 ff0050"
    hPutStrLn stdout $ "g multimedia ff5000"
    hPutStrLn stdout $ "g indicators ff5000"
    --hPutStrLn stdout $ "g arrows 400000"
    hPutStrLn stdout $ "c"
    hFlush stdout
    threadDelay (1000000 `div` 60)
