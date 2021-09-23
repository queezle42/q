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

type Name = [NamePart]
data NamePart
  = MainViewport
  | SingleName
  | IndexName Int
  deriving stock (Eq, Ord, Show)



class IsUI a where
  initialState :: MonadResourceManager m => a -> [NamePart] -> m UIState

class IsState s a | a -> s where
  toState :: a -> State s
  toState = State
  hasUpdate :: a -> STM Bool
  stepState :: MonadResourceManager m => a -> m a
  renderState :: a -> Reader AppState s

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
  initialState (SingletonLayout element) name = initialState element name
  initialState (ListLayout elements) name =
    toState . ListState <$> mapM (\(ix, element) -> initialState element (IndexName ix : name)) (zip [0..] elements)

data ListState = ListState [UIState]
instance IsState (Widget Name) ListState where
  hasUpdate (ListState states) = anyHasUpdates states
  stepState (ListState states) = ListState <$> mapM stepState states
  renderState (ListState states) = vBox <$> mapM renderState states

anyHasUpdates :: [State s] -> STM Bool
anyHasUpdates [] = pure False
anyHasUpdates (x:xs) = hasUpdate x >>= \case
  True -> pure True
  False -> anyHasUpdates xs


-- ** Elements

instance IsUI Element where
  initialState (ContentElement content) = initialState content
  initialState (InteractiveElement interactive) = initialState interactive

-- | Interactive elements which can be selected, activated and optionally capture navigation.
data InteractiveState = InteractiveState UIState Name
instance IsState (Widget Name) InteractiveState where
  hasUpdate (InteractiveState state _) = hasUpdate state
  stepState (InteractiveState state name) = InteractiveState <$> stepState state <*> pure name
  renderState (InteractiveState state name) = do
    AppState{selected} <- ask
    widget <- renderState state
    pure if Just name == selected then withAttr selectedAttr widget else widget

-- ** Content elements

--instance IsUI Content where
--  initialState (Label observable) _name = do
--    state <- newObservableState observable
--    packState $ LabelState state
--
--data LabelState = LabelState (ObservableState String)
--instance IsState (Widget Name) LabelState where
--  hasUpdate (LabelState state) = hasUpdate state
--  stepState (LabelState state) = LabelState <$> stepState state
--  renderState (LabelState state) = strWrap . observableMessageString <$> renderState state
--
--observableMessageString :: ObservableMessage String -> String
--observableMessageString ObservableLoading = "[loading]"
--observableMessageString (ObservableUpdate x) = x
--observableMessageString (ObservableNotAvailable ex) = displayException ex


instance IsUI Content where
  initialState (Label observable) name = do
    state <- newObservableState observable
    packState $ LabelState state name

data LabelState = LabelState (ObservableState String) Name
instance IsState (Widget Name) LabelState where
  hasUpdate (LabelState state _) = hasUpdate state
  stepState (LabelState state name) = LabelState <$> stepState state <*> pure name
  renderState (LabelState state name) = str . observableMessageString <$> renderState state

observableMessageString :: ObservableMessage String -> String
observableMessageString ObservableLoading = "[loading]"
observableMessageString (ObservableUpdate x) = x
observableMessageString (ObservableNotAvailable ex) = displayException ex



-- ** Interactive elements

instance IsUI Interactive where
  initialState (Button content action) name = do
    contentState <- initialState content name
    packState $ ButtonState contentState name action

data ButtonState = ButtonState UIState Name (IO ())
instance IsState (Widget Name) ButtonState where
  hasUpdate (ButtonState contentState name action) = hasUpdate contentState
  stepState (ButtonState contentState name action) = do
    contentState' <- stepState contentState
    pure $ ButtonState contentState' name action
  renderState (ButtonState contentState name _action) = do
    appState <- ask
    widget <- clickable name <$> renderState contentState
    pure $ markHover appState name widget


markHover :: AppState -> Name -> Widget Name -> Widget Name
markHover AppState{mouseDownName} name
  | mouseDownName == Just name = withAttr hoverAttr
  | otherwise = id

-- ** State utilities

data ObservableState a = ObservableState (TVar (Maybe (ObservableMessage a))) (ObservableMessage a)

instance IsState (ObservableMessage a) (ObservableState a) where
  hasUpdate (ObservableState var _) = isJust <$> readTVar var
  stepState state@(ObservableState var value) = do
    liftIO $ atomically do
      readTVar var >>= \case
        Nothing -> pure state
        Just next -> do
          writeTVar var Nothing
          pure $ ObservableState var next
  renderState (ObservableState _ last) = pure last

newObservableState :: MonadResourceManager m => Observable a -> m (ObservableState a)
newObservableState observable = do
  var <- liftIO $ newTVarIO Nothing
  observe observable (liftIO . atomically . writeTVar var . Just)
  pure (ObservableState var ObservableLoading)



-- * Main brick application

main :: IO ()
main = withResourceManagerM $ runUnlimitedAsync do
  withSystemClient \client ->
    runUI =<< exampleUI client

runUI :: MonadResourceManager m => UIRoot -> m ()
runUI ui = do
  uiState <- initialState ui []
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
    appHandleEvent state (MouseDown _vp Vty.BScrollDown [] _loc) = vScrollBy (viewportScroll [MainViewport]) 1 >> continue state
    appHandleEvent state (MouseDown _vp Vty.BScrollUp [] _loc) = vScrollBy (viewportScroll [MainViewport]) (-1) >> continue state

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
mainViewport appState@AppState{uiState} = viewport [MainViewport] Vertical $ runReader (renderState uiState) appState


statusBar :: AppState -> Widget Name
statusBar (AppState{lastEvent, mouseDownName, selected}) = str $
  "Last event: " <> lastEventStr lastEvent <>
  "; mouseDownName: " <> show mouseDownName <>
  "; selected: " <> show selected
  where
    lastEventStr :: Maybe (BrickEvent Name StateEvent) -> String
    lastEventStr Nothing = "[none]"
    lastEventStr (Just ev) = show ev
