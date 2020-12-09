{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Q.G815 (
  run
) where

import Conduit
import Control.Concurrent.MVar
import Control.Monad.State.Lazy
import System.IO (stdout, hFlush)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HM
import Language.Haskell.TH.Syntax (mkName, nameBase)
import Lens.Micro.Platform
import Qd
import Qd.Interface
import Qd.QdProtocol.Client (withConnectTCP)


type Color = Text

data G815 = G815 (MVar G815State) (G815State -> IO ())
data G815State = G815State {
  defaultColor :: Maybe Color,
  groups :: HM.HashMap Text Color,
  keys :: HM.HashMap Text Color
}
  deriving (Eq, Show)

makeLensesWith (lensRules & lensField .~ (\_ _ -> pure . TopName . mkName . ("_" <>) . nameBase)) ''G815State

run :: IO ()
run = withConnectTCP $ \qdInterface -> do
  outboxMVar <- newMVar defaultState
  g815 <- G815 <$> newMVar defaultState <*> return (putMVar outboxMVar)

  join $ runActorSetup qdInterface [] defaultActorConfiguration{actorName=Just "g815"} $ setup g815

  runConduit $ source (takeMVar outboxMVar) .| filterDuplicates .| output
  where
    source :: IO G815State -> ConduitT () G815State IO ()
    source getStateUpdate = forever $ yield =<< liftIO getStateUpdate
    keys :: [Text]
    keys = ["logo", "esc", "g1", "g2", "g3", "g4", "g5"] <> (T.singleton <$> ['a'..'z'] <> ['0'..'9']) <> (T.cons 'f' . T.pack . show <$> ([1..12] :: [Int]))
    setup :: G815 -> ActorSetup (IO ())
    setup g815 = do
      keysSetupAction <- sequence_ <$> traverse setupKey keys
      property <- createProperty "default"
      return $ keysSetupAction >> void (subscribe property $ updateG815 g815 . setDefaultColor . fromRight Nothing . snd)
      where
        setupKey :: Text -> ActorSetup (IO ())
        setupKey key = do
          property <- createProperty key
          return $ void $ subscribe property $ updateG815 g815 . setKey key . fromRight Nothing . snd


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
render G815State{defaultColor, groups, keys} = do
  yield $ "a " <> fromMaybe "000000" defaultColor
  when (not $ HM.member "logo" keys) $ yield "k logo 000000"
  forM_ (HM.toList groups) $ \(key, color) -> yield ("g " <> key <> " " <> color)
  forM_ (HM.toList keys) $ \(key, color) -> yield ("k " <> key <> " " <> color)
