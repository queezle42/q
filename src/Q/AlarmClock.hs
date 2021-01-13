module Q.AlarmClock (
  runDaemon
) where

import Qd
import Qd.QdProtocol.Client

runDaemon :: IO ()
runDaemon = withConnectTCP $ \qdInterface -> do
  undefined
