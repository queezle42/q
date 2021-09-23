module Q.Interface (
  main
) where

import Brick
import Brick.BChan
import Brick.Util qualified
import Brick.Widgets.Border (border)
import Brick.Widgets.Core (joinBorders)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (replicateM)
import Control.Monad.Catch (displayException)
import Control.Monad.Reader
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



-- * Example UI

exampleUI :: MonadAsync m => Client SystemProtocol -> m UIRoot
exampleUI systemClient = do
  idle <- liftIO $ ContentElement . Label . fmap (("System idle: " <>) . show) <$> idle systemClient
  elements <- replicateM 100 do
    label <- Label <$> randomStringObservable
    pure $ InteractiveElement $ Button label (pure ())
  walkers <- replicateM 10 $ ContentElement . Label . fmap show <$> randomWalkObservable
  pure $ UIRoot $ ListLayout $ idle : walkers <> elements


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





-- * Brick application types

data AppState = AppState {
  lastEvent :: Maybe (BrickEvent Name StateEvent),
  uiState :: UIState,
  selected :: Maybe Name,
  initialMouseDownName :: Maybe Name,
  mouseDownName :: Maybe Name,
  notifyChangedStateVar :: TMVar UIState
}
stepUIState :: MonadResourceManager m => AppState -> m AppState
stepUIState appState = do
  nextUIState <- stepState (uiState appState)
  liftIO $ atomically $ putTMVar (notifyChangedStateVar appState) nextUIState
  pure appState {uiState = nextUIState}


data StateEvent = StepStateEvent
  deriving stock Show

data Name = forall s. Name Unique (State s)

newName :: MonadIO m => State s -> m Name
newName state = do
  key <- liftIO newUnique
  pure $ Name key state

getEventHandler :: Name -> EventHandler
getEventHandler (Name _ state) = stateEventHandler state


instance Eq Name where
  (Name x _) == (Name y _) = x == y

instance Ord Name where
  compare (Name x _) (Name y _) = compare x y

instance Show Name where
  show _ = "Name"



class IsUI a where
  initialState :: MonadResourceManager m => EventHandler -> a -> m UIState


data EventHandler = EventHandler {
  scrollUp :: EventM Name (),
  scrollDown :: EventM Name (),
  navigateUp :: EventM Name (),
  navigateDown :: EventM Name (),
  navigateOut :: EventM Name (),
  navigateIn :: EventM Name (),
  activateAction :: EventM Name ()
}

emptyEventHandler :: EventHandler
emptyEventHandler =
  EventHandler {
    scrollUp = pure (),
    scrollDown = pure (),
    navigateUp = pure (),
    navigateDown = pure (),
    navigateOut = pure (),
    navigateIn = pure (),
    activateAction = pure ()
  }


class IsState s a | a -> s where
  toState :: a -> State s
  toState = State
  mapState :: (s -> t) -> a -> State t
  mapState fn = toState . MappedState fn
  stateEventHandler :: a -> EventHandler
  hasUpdate :: a -> STM Bool
  stepState :: MonadResourceManager m => a -> m a
  renderState :: a -> Reader AppState s

type UIState = State (Widget Name)


-- | Quantification wrapper for 'IsState'
data State s = forall a. IsState s a => State a
instance IsState s (State s) where
  toState = id
  mapState fn (State x) = mapState fn x
  hasUpdate (State x) = hasUpdate x
  stepState (State x) = State <$> stepState x
  renderState (State x) = renderState x
  stateEventHandler (State x) = stateEventHandler x

toStateM :: (IsState s a, Monad m) => a -> m (State s)
toStateM = pure . toState


instance Functor State where
  fmap = mapState

data MappedState s = forall t a. IsState t a => MappedState (t -> s) a
instance IsState s (MappedState s) where
  hasUpdate (MappedState _ x) = hasUpdate x
  mapState fnew (MappedState fn x) = toState $ MappedState (fnew . fn) x
  stepState (MappedState fn x) = MappedState fn <$> stepState x
  renderState (MappedState fn x) = fn <$> renderState x
  stateEventHandler (MappedState _ x) = stateEventHandler x


-- | State with a sub-'ResourceManager', i.e. a subtree that can be disposed.
data SubState s = forall a. IsState s a => SubState ResourceManager a
instance IsState s (SubState s) where
  hasUpdate (SubState _rm x) = hasUpdate x
  stepState (SubState rm x) = localResourceManager rm $ SubState rm <$> stepState x
  renderState (SubState _rm x) = renderState x
  stateEventHandler (SubState _rm x) = stateEventHandler x
instance IsResourceManager (SubState s) where
  toResourceManager (SubState rm _) = rm
instance IsDisposable (SubState s) where
  toDisposable = toDisposable . toResourceManager

--subState :: (IsState a, MonadResourceManager m) => a -> m State
--subState x = do
--  rm <- undefined
--  pure $ State rm x


instance IsUI UIRoot where
  initialState parentEv (UIRoot layout) = do
    mfix \state -> do
      name <- newName state
      let ev = pageViewportEvents name parentEv
      contentState <- initialState ev layout
      toStateM $ PageViewport ev name contentState

data PageViewport = PageViewport EventHandler Name UIState
instance IsState (Widget Name) PageViewport where
  hasUpdate (PageViewport _ name state) = hasUpdate state
  stepState (PageViewport ev name state) = PageViewport ev name <$> stepState state
  renderState (PageViewport _ name state) = viewport name Vertical <$> renderState state
  stateEventHandler (PageViewport ev _ _) = ev

pageViewportEvents :: Name -> EventHandler -> EventHandler
pageViewportEvents name ev =
  ev {
    scrollUp = vScrollBy (viewportScroll name) 1,
    scrollDown = vScrollBy (viewportScroll name) (-1)
  }

-- ** Layout elements

instance IsUI Layout where
  initialState ev (SingletonLayout element) = initialState ev element
  initialState ev (ListLayout elements) =
    toState . ListState ev <$> mapM (initialState ev) elements

data ListState = ListState EventHandler [UIState]
instance IsState (Widget Name) ListState where
  hasUpdate (ListState ev states) = anyHasUpdates states
  stepState (ListState ev states) = ListState ev <$> mapM stepState states
  renderState (ListState ev states) = vBox <$> mapM renderState states
  stateEventHandler (ListState ev states) = ev

anyHasUpdates :: [State s] -> STM Bool
anyHasUpdates [] = pure False
anyHasUpdates (x:xs) = hasUpdate x >>= \case
  True -> pure True
  False -> anyHasUpdates xs


-- ** Elements

instance IsUI Element where
  initialState ev (ContentElement content) = initialState ev content
  initialState ev (InteractiveElement interactive) = initialState ev interactive

-- | Interactive elements which can be selected, activated and optionally capture navigation.
data InteractiveState = InteractiveState EventHandler Name UIState
instance IsState (Widget Name) InteractiveState where
  hasUpdate (InteractiveState ev _ state) = hasUpdate state
  stepState (InteractiveState ev name state) = InteractiveState ev name <$> stepState state
  renderState (InteractiveState ev name state) = do
    AppState{selected} <- ask
    widget <- renderState state
    pure if Just name == selected then withAttr selectedAttr widget else widget
  stateEventHandler (InteractiveState ev _ _) = ev

-- ** Content elements

instance IsUI Content where
  initialState ev (Label observable) = labelWidget <<$>> newObservableState ev observable

labelWidget :: ObservableMessage String -> Widget Name
labelWidget = str . observableMessageToString
  where
    observableMessageToString :: ObservableMessage String -> String
    observableMessageToString ObservableLoading = "[loading]"
    observableMessageToString (ObservableUpdate x) = x
    observableMessageToString (ObservableNotAvailable ex) = displayException ex



-- ** Interactive elements

instance IsUI Interactive where
  initialState ev (Button content action) = do
    contentState <- initialState ev content
    mfix \state -> do
      name <- newName state
      toStateM $ ButtonState ev contentState name action

data ButtonState = ButtonState EventHandler UIState Name (IO ())
instance IsState (Widget Name) ButtonState where
  hasUpdate (ButtonState ev contentState name action) = hasUpdate contentState
  stepState (ButtonState ev contentState name action) = do
    contentState' <- stepState contentState
    pure $ ButtonState ev contentState' name action
  renderState (ButtonState ev contentState name _action) = do
    appState <- ask
    widget <- clickable name <$> renderState contentState
    pure $ markHover appState name widget
  stateEventHandler (ButtonState ev _ _ _) = ev


markHover :: AppState -> Name -> Widget Name -> Widget Name
markHover AppState{mouseDownName} name
  | mouseDownName == Just name = withAttr hoverAttr
  | otherwise = id

-- ** State utilities

data ObservableState a = ObservableState EventHandler (TVar (Maybe (ObservableMessage a))) (ObservableMessage a)

instance IsState (ObservableMessage a) (ObservableState a) where
  hasUpdate (ObservableState _ var _) = isJust <$> readTVar var
  stepState state@(ObservableState ev var value) = do
    liftIO $ atomically do
      readTVar var >>= \case
        Nothing -> pure state
        Just next -> do
          writeTVar var Nothing
          pure $ ObservableState ev var next
  renderState (ObservableState _ _ last) = pure last
  stateEventHandler (ObservableState ev _ _) = ev

newObservableState :: MonadResourceManager m => EventHandler -> Observable a -> m (State (ObservableMessage a))
newObservableState ev observable = do
  var <- liftIO $ newTVarIO Nothing
  observe observable (liftIO . atomically . writeTVar var . Just)
  toStateM (ObservableState ev var ObservableLoading)



-- * Main brick application

main :: IO ()
main = withResourceManagerM $ runUnlimitedAsync do
  withSystemClient \client ->
    runUI =<< exampleUI client

runUI :: MonadResourceManager m => UIRoot -> m ()
runUI ui = do
  uiState <- initialState emptyEventHandler ui
  notifyChangedStateVar <- liftIO $ newEmptyTMVarIO
  let initialAppState = AppState {
    lastEvent = Nothing,
    uiState,
    selected = Nothing,
    initialMouseDownName = Nothing,
    mouseDownName = Nothing,
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
    appHandleEvent state (MouseDown name Vty.BScrollDown [] _loc) =
      scrollUp (getEventHandler name) >> continue state
    appHandleEvent state (MouseDown name Vty.BScrollUp [] _loc) =
      scrollDown (getEventHandler name) >> continue state

    -- Mouse events, focus events
    appHandleEvent state@AppState{initialMouseDownName = Nothing} (MouseDown name Vty.BLeft [] _loc) =
      continue state {
        initialMouseDownName = Just name,
        mouseDownName = Just name
      }
    appHandleEvent state@AppState{initialMouseDownName} (MouseDown name Vty.BLeft [] _loc) =
      continue state {
        mouseDownName = if initialMouseDownName == Just name then Just name else Nothing
      }
    appHandleEvent state (MouseUp name (Just Vty.BLeft) _loc) = continue (resetMouseEvents state)
    appHandleEvent state (MouseUp name Nothing _loc) = fail "MouseUp without button registered, why did this happen?"
    appHandleEvent state (VtyEvent (Vty.EvLostFocus)) = continue (resetMouseEvents state)
    appHandleEvent state (VtyEvent (Vty.EvGainedFocus)) = continue (resetMouseEvents state)

    -- Navigation
    appHandleEvent state (VtyEvent (Vty.EvKey Vty.KDown [])) = continue =<< navDown state
    appHandleEvent state (VtyEvent (Vty.EvKey Vty.KUp [])) = continue =<< navUp state
    appHandleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'j') [])) = continue =<< navDown state
    appHandleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'k') [])) = continue =<< navUp state

    -- Exit when pressing 'q'
    appHandleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt state
    -- Exit when pressing <ctrl-c>
    appHandleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl])) = halt state
    -- Ignore other events
    appHandleEvent state event = continue state

    appStartEvent :: AppState -> EventM Name AppState
    appStartEvent = stepState

    appAttrMap :: AppState -> AttrMap
    appAttrMap _state = attrMap Vty.defAttr [
      (hoverAttr, Brick.Util.bg Vty.brightBlack),
      (selectedAttr, Brick.Util.bg Vty.brightBlack)
      ]

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

resetMouseEvents :: AppState -> AppState
resetMouseEvents state =
  state {
    initialMouseDownName = Nothing,
    mouseDownName = Nothing
  }

-- ** Attributes

hoverAttr = attrName "hover"
selectedAttr = attrName "selected"


-- ** Navigation

navUp :: AppState -> EventM Name AppState
navUp = pure

navDown :: AppState -> EventM Name AppState
navDown = pure

-- ** Rendering

mainLayout :: AppState -> Widget Name
mainLayout state = mainViewport state <=> statusBar state

mainViewport :: AppState -> Widget Name
mainViewport appState@AppState{uiState} = runReader (renderState uiState) appState


statusBar :: AppState -> Widget Name
statusBar (AppState{lastEvent, mouseDownName, selected}) = str $
  "Last event: " <> lastEventStr lastEvent <>
  "; mouseDownName: " <> show mouseDownName <>
  "; selected: " <> show selected
  where
    lastEventStr :: Maybe (BrickEvent Name StateEvent) -> String
    lastEventStr Nothing = "[none]"
    lastEventStr (Just ev) = show ev
