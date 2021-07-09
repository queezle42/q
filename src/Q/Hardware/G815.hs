{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

-- For rpc:
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Q.Hardware.G815 (
  run,
  runSetIdle,
) where

import Conduit
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad.State.Lazy
import System.IO (stdout, hFlush)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Tuple (swap)
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HM
import Language.Haskell.TH.Syntax (mkName, nameBase)
import Lens.Micro.Platform
import Network.Rpc
import Network.Rpc.SocketLocation


type Color = Text

data G815 = G815 (MVar G815State) (G815State -> IO ())
data G815State = G815State {
  idle :: Bool,
  defaultColor :: Maybe Color,
  groups :: HM.HashMap Text Color,
  keys :: HM.HashMap Text Color
}
  deriving (Eq, Show)

$(makeLensesWith (lensRules & lensField .~ (\_ _ -> pure . TopName . mkName . ("_" <>) . nameBase)) ''G815State)

$(makeRpc $ rpcApi "G815" [
    rpcFunction "setIdle" $ do
      addArgument "idle" [t|Bool|]
  ]
 )

socketLocation :: IO FilePath
socketLocation = sessionSocketPath "q-g815"


withRpcClient :: (Client G815Protocol -> IO a) -> IO a
withRpcClient action = do
  loc <- socketLocation
  withClientUnix loc action

runSetIdle :: Bool -> IO ()
runSetIdle value = withRpcClient $ \client -> setIdle client value

run :: IO ()
run = do
  outboxMVar <- newMVar defaultState
  g815 <- G815 <$> newMVar defaultState <*> return (putMVar outboxMVar)

  rpcServerTask <- async $ listenUnix @G815Protocol (rpcImpl g815) =<< socketLocation
  renderTask <- async $ runConduit $ source (takeMVar outboxMVar) .| filterDuplicates .| output

  void $ waitAnyCancel [renderTask, rpcServerTask]
  where
    source :: IO G815State -> ConduitT () G815State IO ()
    source getStateUpdate = forever $ yield =<< liftIO getStateUpdate

rpcImpl :: G815 -> G815ProtocolImpl
rpcImpl g815 = G815ProtocolImpl {
  setIdleImpl = updateG815 g815 . assign _idle
}


updateG815' :: G815 -> (G815State -> (G815State, a)) -> IO a
updateG815' (G815 stateMVar renderState) fn = do
  modifyMVar stateMVar $ \oldState -> do
    let (newState, x) = fn oldState
    renderState newState
    return (newState, x)

updateG815 :: G815 -> State G815State a -> IO a
updateG815 g815 = updateG815' g815 . fmap swap . runState

setDefaultColor :: Maybe Color -> State G815State ()
setDefaultColor = assign _defaultColor

setKey :: Text -> Maybe Color -> State G815State ()
setKey key color = _keys . at key .= color


defaultState :: G815State
defaultState = G815State {
  idle = False,
  defaultColor = Nothing,
  groups = HM.fromList [("multimedia", "ff5000"), ("indicators", "ff5000")],
  keys = HM.empty
}

filterDuplicates :: forall a. Eq a => ConduitT a a IO ()
filterDuplicates = do
  first <- await
  case first of
    Just first' -> yield first' >> filterDuplicates' first'
    Nothing -> return ()
  where
    filterDuplicates' :: a -> ConduitT a a IO ()
    filterDuplicates' previous = do
      next <- await
      case next of
        Just next' -> do
          when (previous /= next') $ yield next'
          filterDuplicates' next'
        Nothing -> return ()

output :: ConduitT G815State Void IO ()
output = awaitForever $ \s -> render s .| outputFrame
  where
    outputFrame :: ConduitT Text Void IO ()
    outputFrame = do
      awaitForever $ liftIO . T.hPutStrLn stdout
      liftIO $ T.hPutStrLn stdout "c" -- Commit
      liftIO $ hFlush stdout

render :: Monad m => G815State -> ConduitT i Text m ()
render G815State{idle, defaultColor, groups, keys} = if idle
  then yield "a 000000"
  else do
    yield $ "a " <> fromMaybe "ff0000" defaultColor
    unless (HM.member "logo" keys) $ yield "k logo 000000"
    forM_ (HM.toList groups) $ \(key, color) -> yield ("g " <> key <> " " <> color)
    forM_ (HM.toList keys) $ \(key, color) -> yield ("k " <> key <> " " <> color)
