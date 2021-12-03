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
  execSystemDaemon,
  execSetIdle,
  execWatchIdle,
  withSystemClient,
  SystemProtocol,
  idle,
) where

import Quasar.Network
import Quasar.Network.TH
import Quasar.Observable
import Quasar.Prelude
import Quasar.ResourceManager
import System.Systemd.Daemon (getActivatedSockets)
import Data.ByteString.Char8 as BS8

import System.IO (stdout, stderr)


$(makeRpc $ rpcApi "System" $ do
    rpcFunction "setIdle" $ do
      addArgument "idle" [t|Bool|]
    rpcObservable "idle" [t|Bool|]
 )

unixSocketLocation = "/run/q/socket"

data Handle = Handle {
  idleVar :: ObservableVar Bool
}

execSystemDaemon :: IO ()
execSystemDaemon = do
  listeners <- getActivatedSockets >>= \case
    Nothing -> fail "No sockets were provided via socket activation"
    Just activatedSockets -> pure $ ListenSocket <$> activatedSockets

  --let listeners = [ UnixSocket unixSocketLocation ]

  handle <- newHandle

  withRootResourceManager do
    observe (idleVar handle) $ \case
      ObservableLoading -> liftIO $ hPutStrLn stderr "Idle loading"
      ObservableUpdate val -> liftIO $ hPutStrLn stderr $ BS8.pack $ "Idle updated: " <> show val
      ObservableNotAvailable ex -> liftIO $ hPutStrLn stderr $ BS8.pack $ "Idle error: " <> show ex

    runServer @SystemProtocol (rpcImpl handle) listeners

execSetIdle :: Bool -> IO ()
execSetIdle value = withRootResourceManager $ withSystemClient $ \client -> setIdle client value

execWatchIdle :: IO ()
execWatchIdle = withRootResourceManager $ withSystemClient \client -> do
  idleObservable <- liftIO $ idle client
  observeBlocking (idleObservable) $ \case
    ObservableLoading -> liftIO $ hPutStrLn stderr "Idle loading"
    ObservableUpdate val -> liftIO $ hPutStrLn stdout $ BS8.pack $ show val
    ObservableNotAvailable ex -> liftIO $ hPutStrLn stderr $ BS8.pack $ "Idle error: " <> show ex

withSystemClient :: MonadResourceManager m => (Client SystemProtocol -> m a) -> m a
withSystemClient = withClientUnix @SystemProtocol unixSocketLocation

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
