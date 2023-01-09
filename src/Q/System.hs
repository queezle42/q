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

import Control.Monad.Catch
import Quasar
import Quasar.Network
import Quasar.Network.TH
import Quasar.Prelude
import System.Systemd.Daemon (getActivatedSockets)
import Data.ByteString.Char8 as BS8

import System.IO (stdout, stderr)


$(makeRpc $ rpcApi "System" $ do
    rpcFunction "setIdle" $ do
      addArgument "idle" [t|Bool|]
    rpcObservable "idle" [t|Bool|]
 )

unixSocketLocation :: FilePath
unixSocketLocation = "/run/q/socket"

data Handle = Handle {
  idleVar :: ObservableVar Bool
}

execSystemDaemon :: QuasarIO ()
execSystemDaemon = do
  listeners <- liftIO $ getActivatedSockets >>= \case
    Nothing -> fail "No sockets were provided via socket activation"
    Just activatedSockets -> pure $ ListenSocket <$> activatedSockets

  --let listeners = [ UnixSocket unixSocketLocation ]

  handle <- liftIO newHandle

  async_ $ observeBlocking (idleVar handle) $ \case
    ObservableLoading -> liftIO $ hPutStrLn stderr "Idle loading"
    ObservableValue val -> liftIO $ hPutStrLn stderr $ BS8.pack $ "Idle updated: " <> show val
    ObservableNotAvailable ex -> liftIO $ hPutStrLn stderr $ BS8.pack $ "Idle error: " <> show ex

  runServer @SystemProtocol (rpcImpl handle) listeners

execSetIdle :: Bool -> QuasarIO ()
execSetIdle value = withSystemClient $ \client -> setIdle client value

execWatchIdle :: QuasarIO ()
execWatchIdle = withSystemClient \client -> do
  idleObservable <- idle client
  observeBlocking idleObservable $ \case
    ObservableLoading -> liftIO $ hPutStrLn stderr "Idle loading"
    ObservableValue val -> liftIO $ hPutStrLn stdout $ BS8.pack $ show val
    ObservableNotAvailable ex -> liftIO $ hPutStrLn stderr $ BS8.pack $ "Idle error: " <> show ex

withSystemClient :: (MonadQuasar m, MonadIO m, MonadMask m) => (Client SystemProtocol -> m a) -> m a
withSystemClient = withClientUnix @SystemProtocol unixSocketLocation

newHandle :: IO Handle
newHandle = do
  idleVar <- newObservableVarIO False
  pure Handle {
    idleVar
  }


rpcImpl :: Handle -> SystemProtocolImpl
rpcImpl handle = SystemProtocolImpl {
  setIdleImpl = atomically . setObservableVar (idleVar handle),
  idleImpl = toObservable (idleVar handle)
}
