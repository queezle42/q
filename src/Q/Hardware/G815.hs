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
) where

import Conduit as C
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad.State.Lazy
import System.IO (stdout, hFlush)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Text.IO qualified as T
import Data.HashMap.Strict qualified as HM
import Language.Haskell.TH.Syntax (mkName, nameBase)
import Lens.Micro.Platform
import Q.System
import Quasar
import Quasar.Prelude


type Color = Text

data G815 = G815 (MVar G815State) (G815State -> IO ())
data G815State = G815State {
  systemIsIdle :: Bool,
  defaultColor :: Maybe Color,
  groups :: HM.HashMap Text Color,
  keys :: HM.HashMap Text Color
}
  deriving (Eq, Show)

$(makeLensesWith (lensRules & lensField .~ (\_ _ -> pure . TopName . mkName . ("_" <>) . nameBase)) ''G815State)


run :: QuasarIO ()
run = do
  outboxMVar <- liftIO $ newMVar defaultState
  g815 <- liftIO $ G815 <$> newMVar defaultState <*> return (putMVar outboxMVar)

  withSystemClient $ \client -> do
    idleObservable <- idle client

    async_ $ observeBlocking idleObservable $ \msg -> do
      let value = case msg of
            ObservableValue r -> Just r
            _ -> Nothing
      liftIO $ updateG815 g815 $ assign _systemIsIdle (fromMaybe False value)

    liftIO $ runConduit $ source (takeMVar outboxMVar) .| filterDuplicates .| output
  where
    source :: IO G815State -> ConduitT () G815State IO ()
    source getStateUpdate = forever $ yield =<< liftIO getStateUpdate

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
  systemIsIdle = False,
  defaultColor = Nothing,
  groups = HM.fromList [("multimedia", "ff5000"), ("indicators", "ff5000")],
  keys = HM.empty
}

filterDuplicates :: forall a. Eq a => ConduitT a a IO ()
filterDuplicates = do
  first <- C.await
  case first of
    Just first' -> yield first' >> filterDuplicates' first'
    Nothing -> return ()
  where
    filterDuplicates' :: a -> ConduitT a a IO ()
    filterDuplicates' previous = do
      next <- C.await
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
render G815State{systemIsIdle, defaultColor, groups, keys} = if systemIsIdle
  then yield "a 000000"
  else do
    yield $ "a " <> fromMaybe "ff0000" defaultColor
    unless (HM.member "logo" keys) $ yield "k logo 000000"
    forM_ (HM.toList groups) $ \(key, color) -> yield ("g " <> key <> " " <> color)
    forM_ (HM.toList keys) $ \(key, color) -> yield ("k " <> key <> " " <> color)
