{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

-- For rpc:
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Q.System (
  runSystemDaemon
) where

import Quasar.Network
import Quasar.Network.TH
import Quasar.Observable
import Quasar.Prelude
import System.Systemd.Daemon (getActivatedSockets)


$(makeRpc $ rpcApi "System" $ do
    rpcFunction "setIdle" $ do
      addArgument "idle" [t|Bool|]
    rpcObservable "idle" [t|Bool|]
 )

data Handle = Handle {
  idleVar :: ObservableVar Bool
}

runSystemDaemon :: IO ()
runSystemDaemon = do
  listeners <- getActivatedSockets >>= \case
    Nothing -> fail "No sockets were provided via socket activation"
    Just activatedSockets -> pure $ ListenSocket <$> activatedSockets

  handle <- newHandle

  runServer @SystemProtocol (rpcImpl handle) listeners

withSystemClient :: (Client SystemProtocol -> IO a) -> IO a
withSystemClient = withClientUnix @SystemProtocol "path"

newHandle :: IO Handle
newHandle = do
  idleVar <- newObservableVar False
  pure Handle {
    idleVar
  }


rpcImpl :: Handle -> SystemProtocolImpl
rpcImpl handle = SystemProtocolImpl {
  setIdleImpl = liftIO . setObservableVar (idleVar handle),
  idleImpl = toObservable (idleVar handle)
}
