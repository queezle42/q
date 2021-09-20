module Q.Interface (
  main
) where

import Brick
import Brick.BChan
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (replicateM)
import Control.Monad.Catch (displayException)
import Data.List (intersperse)
import Data.Maybe (isJust)
import qualified Graphics.Vty as Vty
import Q.System
import Quasar.Async
import Quasar.Disposable
import Quasar.Network
import Quasar.Observable
import Quasar.Prelude
import Quasar.ResourceManager
import System.Random (randomRIO)


-- * Observable stuff

--data NamedObservable k r = NamedObservable k (Observable r)
--
--instance IsRetrievable r (NamedObservable k r) where
--  retrieve = retrieve . toObservable
--
--instance IsObservable r (NamedObservable k r) where
--  toObservable (NamedObservable _ observable) = observable
--
--instance Eq k => Eq (NamedObservable k r) where
--  NamedObservable x _ == NamedObservable y _ = x == y



-- * UI definition

type Key = Unique


data UIRoot = UIRoot Layout

data Layout where
  SingletonLayout :: Element -> Layout
  ListLayout :: [Element] -> Layout

data Element where
  ContentElement :: Content -> Element
  InteractiveElement :: Interactive -> Element

data Content where
  Label :: Observable String -> Content

data Interactive where
  Button :: Content -> IO () -> Interactive



-- * Brick application types

data AppState = AppState {
  lastEvent :: Maybe (BrickEvent Name StateEvent),
  uiState :: UIState,
  notifyChangedStateVar :: TMVar UIState
}
stepUIState :: MonadResourceManager m => AppState -> m AppState
stepUIState appState = do
  nextUIState <- stepState (uiState appState)
  liftIO $ atomically $ putTMVar (notifyChangedStateVar appState) nextUIState
  pure appState {uiState = nextUIState}

data StateEvent = StepStateEvent
  deriving stock Show

data Name = MainViewport
  deriving stock (Eq, Ord, Show)


class IsUI a where
  initialState :: MonadResourceManager m => a -> m UIState

class IsState s a | a -> s where
  toState :: a -> State s
  toState = State
  hasUpdate :: a -> STM Bool
  stepState :: MonadResourceManager m => a -> m a
  renderState :: a -> s

type UIState = State (Widget Name)

-- | Quantification wrapper for 'IsState'
data State s = forall a. IsState s a => State a
instance IsState s (State s) where
  toState = id
  hasUpdate (State x) = hasUpdate x
  stepState (State x) = State <$> stepState x
  renderState (State x) = renderState x

packState :: (IsState s a, Monad m) => a -> m (State s)
packState = pure . toState


-- | State with a sub-'ResourceManager', i.e. a subtree that can be disposed.
data SubState s = forall a. IsState s a => SubState ResourceManager a
instance IsState s (SubState s) where
  hasUpdate (SubState rm x) = hasUpdate x
  stepState (SubState rm x) = localResourceManager rm $ SubState rm <$> stepState x
  renderState (SubState rm x) = renderState x
instance IsResourceManager (SubState s) where
  toResourceManager (SubState rm _) = rm
instance IsDisposable (SubState s) where
  toDisposable = toDisposable . toResourceManager

--subState :: (IsState a, MonadResourceManager m) => a -> m State
--subState x = do
--  rm <- undefined
--  pure $ State rm x


instance IsUI UIRoot where
  initialState (UIRoot layout) = initialState layout

-- ** Layout elements

instance IsUI Layout where
  initialState (SingletonLayout element) = initialState element
  initialState (ListLayout elements) = toState . ListState <$> mapM initialState elements

data ListState = ListState [UIState]
instance IsState (Widget Name) ListState where
  hasUpdate (ListState states) = anyHasUpdates states
  stepState (ListState states) = ListState <$> mapM stepState states
  renderState (ListState states) = vBox $ map renderState states

anyHasUpdates :: [State s] -> STM Bool
anyHasUpdates [] = pure False
anyHasUpdates (x:xs) = hasUpdate x >>= \case
  True -> pure True
  False -> anyHasUpdates xs


-- ** Elements

instance IsUI Element where
  initialState (ContentElement content) = initialState content
  initialState (InteractiveElement interactive) = initialState interactive

-- ** Content elements

instance IsUI Content where
  initialState (Label observable) = do
    state <- newObservableState observable
    packState $ LabelState state

newtype LabelState = LabelState (ObservableState String)
instance IsState (Widget Name) LabelState where
  hasUpdate (LabelState state) = hasUpdate state
  stepState (LabelState state) = LabelState <$> stepState state
  renderState (LabelState state) = strWrap $ observableMessageString $ renderState state

observableMessageString :: ObservableMessage String -> String
observableMessageString ObservableLoading = "[loading]"
observableMessageString (ObservableUpdate x) = x
observableMessageString (ObservableNotAvailable ex) = displayException ex



-- ** Interactive elements

instance IsUI Interactive where
  initialState (Button content action) = do
    contentState <- initialState content
    packState $ ButtonState contentState action

data ButtonState = ButtonState UIState (IO ())
instance IsState (Widget Name) ButtonState where
  hasUpdate (ButtonState contentState action) = hasUpdate contentState
  stepState (ButtonState contentState action) = do
    contentState' <- stepState contentState
    pure $ ButtonState (contentState') action
  renderState (ButtonState _contentState _action) = undefined


-- ** State utilities

data ObservableState a = ObservableState (TVar (Maybe (ObservableMessage a))) (ObservableMessage a)

instance IsState (ObservableMessage a) (ObservableState a) where
  hasUpdate :: ObservableState a -> STM Bool
  hasUpdate (ObservableState var _) = isJust <$> readTVar var
  stepState :: MonadResourceManager m => ObservableState a -> m (ObservableState a)
  stepState state@(ObservableState var value) = do
    liftIO $ atomically do
      readTVar var >>= \case
        Nothing -> pure state
        Just next -> do
          writeTVar var Nothing
          pure $ ObservableState var next
  renderState :: ObservableState a -> ObservableMessage a
  renderState (ObservableState _ last) = last

newObservableState :: MonadResourceManager m => Observable a -> m (ObservableState a)
newObservableState observable = do
  var <- liftIO $ newTVarIO Nothing
  observe observable (liftIO . atomically . writeTVar var . Just)
  pure (ObservableState var ObservableLoading)



-- * Example UI

exampleUI :: MonadAsync m => Client SystemProtocol -> m UIRoot
exampleUI systemClient = do
  idle <- liftIO $ ContentElement . Label . fmap (("System idle: " <>) . show) <$> idle systemClient
  walkers <- replicateM 10 (ContentElement . Label . fmap show <$> randomWalkObservable)
  randoms <- replicateM 100 (ContentElement . Label <$> randomStringObservable)
  pure $ UIRoot $ ListLayout $ idle : walkers <> randoms

randomStringObservable :: MonadAsync m => m (Observable String)
randomStringObservable = do
  var <- liftIO $ newObservableVar "[loading]"
  async_ $ liftIO $ forever do
    amount <- randomRIO (10, 60)
    setObservableVar var =<< replicateM amount (randomRIO ('0', 'z'))
    threadDelay =<< randomRIO (1000000, 10000000)
  pure $ toObservable var

randomWalkObservable :: MonadAsync m => m (Observable Int)
randomWalkObservable = do
  var <- liftIO $ newObservableVar 0
  async_ $ liftIO $ forever do
    modifyObservableVar_ var $ \x -> (x +) <$> randomRIO (-10, 10)
    threadDelay =<< randomRIO (1000000, 2000000)
  pure $ toObservable var


-- * Main brick application

main :: IO ()
main = withResourceManagerM $ runUnlimitedAsync do
  withSystemClient \client ->
    runUI =<< exampleUI client

runUI :: MonadResourceManager m => UIRoot -> m ()
runUI ui = do
  uiState <- initialState ui
  notifyChangedStateVar <- liftIO $ newEmptyTMVarIO
  let initialAppState = AppState {
    lastEvent = Nothing,
    uiState,
    notifyChangedStateVar
  }

  withSubResourceManagerM do
    rm <- askResourceManager
    eventChan <- liftIO $ newBChan 1

    runUnlimitedAsync $ async $ liftIO $
      notifyChangedStateThread notifyChangedStateVar eventChan

    liftIO do
      initialVty <- buildVty
      void $ customMain initialVty buildVty (Just eventChan) (app rm) initialAppState
  where
    buildVty :: IO Vty.Vty
    buildVty = do
      vty <- Vty.mkVty =<< Vty.standardIOConfig
      let output = Vty.outputIface vty
      when (Vty.supportsMode output Vty.Mouse) $
        Vty.setMode output Vty.Mouse True
      when (Vty.supportsMode output Vty.BracketedPaste) $
        Vty.setMode output Vty.BracketedPaste True
      when (Vty.supportsMode output Vty.Focus) $
        Vty.setMode output Vty.Focus True
      when (Vty.supportsMode output Vty.Hyperlink) $
        Vty.setMode output Vty.Hyperlink True
      pure vty

    notifyChangedStateThread :: TMVar (State s) -> BChan StateEvent -> IO ()
    notifyChangedStateThread stateVar eventChan = forever do
      atomically $ takeTMVar stateVar >>= hasUpdate >>= (`unless` retry)
      writeBChan eventChan StepStateEvent


app :: ResourceManager -> App AppState StateEvent Name
app rm = App { appDraw, appChooseCursor, appHandleEvent = debugEvents appHandleEvent, appStartEvent, appAttrMap }
  where
    -- * App fields

    appDraw :: AppState -> [Widget Name]
    appDraw state = [mainLayout state]

    appChooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
    appChooseCursor _state _locations = Nothing

    appHandleEvent :: AppState -> BrickEvent Name StateEvent -> EventM Name (Next AppState)
    -- Handle observable updates
    appHandleEvent state (AppEvent StepStateEvent) = continue =<< stepState state

    -- Scroll main viewport
    appHandleEvent state (MouseDown vp Vty.BScrollDown [] _loc) = vScrollBy (viewportScroll vp) 1 >> continue state
    appHandleEvent state (MouseDown vp Vty.BScrollUp [] _loc) = vScrollBy (viewportScroll vp) (-1) >> continue state

    -- Exit when pressing 'q'
    appHandleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt state
    -- Exit when pressing <ctrl-c>
    appHandleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl])) = halt state
    -- Ignore other events
    appHandleEvent state event = continue state

    appStartEvent :: AppState -> EventM Name AppState
    appStartEvent = stepState

    appAttrMap :: AppState -> AttrMap
    appAttrMap _state = attrMap Vty.defAttr []

    -- * Other

    debugEvents
      :: (AppState -> BrickEvent Name StateEvent -> EventM Name (Next AppState))
      -> AppState
      -> BrickEvent Name StateEvent
      -> EventM Name (Next AppState)
    debugEvents handler state event@(AppEvent StepStateEvent) = handler state event
    debugEvents handler state event = handler (state {lastEvent = Just event}) event

    stepState :: AppState -> EventM Name AppState
    stepState state = liftIO (onResourceManager rm (stepUIState state))


mainLayout :: AppState -> Widget Name
mainLayout state = mainViewport state <=> statusBar state

mainViewport :: AppState -> Widget Name
mainViewport AppState{uiState} = viewport MainViewport Vertical $ renderState uiState


statusBar :: AppState -> Widget Name
statusBar (AppState{lastEvent}) = str $ "Last event: " <> lastEventStr lastEvent
  where
    lastEventStr :: Maybe (BrickEvent Name StateEvent) -> String
    lastEventStr Nothing = "[none]"
    lastEventStr (Just ev) = show ev
