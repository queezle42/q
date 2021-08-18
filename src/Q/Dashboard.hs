module Q.Dashboard where

import Brick
import Control.Monad (when)
--import Control.Monad.IO.Class (liftIO)
import Data.Text
import qualified Graphics.Vty as Vty
import Prelude

data State = State {
  info :: Text,
  lastEvent :: Maybe (BrickEvent Name Event)
}
data Event = Event
  deriving (Eq, Ord, Show)
data Name = MainViewport
  deriving (Eq, Ord, Show)

run :: IO ()
run = do
  let initialState = State {info="initialized", lastEvent=Nothing}
  initialVty <- buildVty
  _finalState <- customMain initialVty buildVty Nothing app initialState
  return ()
  where
    buildVty :: IO Vty.Vty
    buildVty = do
      vty <- Vty.mkVty =<< Vty.standardIOConfig
      let output = Vty.outputIface vty
      when (Vty.supportsMode output Vty.Mouse) $
        Vty.setMode output Vty.Mouse True
      return vty


app :: App State Event Name
app = App { appDraw, appChooseCursor, appHandleEvent, appStartEvent, appAttrMap }
  where
    appDraw :: State -> [Widget Name]
    appDraw state = [mainLayout state]

    appChooseCursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
    appChooseCursor _state _locations = Nothing

    appHandleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
    appHandleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt state
    -- Handle <ctrl-c>
    appHandleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl])) = halt state
    appHandleEvent state (VtyEvent (Vty.EvKey (Vty.KChar char) [])) = continue $ state{info=singleton char, lastEvent=Nothing}

    appHandleEvent state (MouseDown vp Vty.BScrollDown [] _loc) = vScrollBy (viewportScroll vp) 1 >> continue state{lastEvent=Nothing}
    appHandleEvent state (MouseDown vp Vty.BScrollUp [] _loc) = vScrollBy (viewportScroll vp) (-1) >> continue state{lastEvent=Nothing}

    appHandleEvent state event = continue state {lastEvent=Just event}

    appStartEvent :: State -> EventM Name State
    appStartEvent state = return state

    appAttrMap :: State -> AttrMap
    appAttrMap _state = attrMap Vty.defAttr []

mainLayout :: State -> Widget Name
mainLayout state = mainViewport state <=> statusBar state

mainViewport :: State -> Widget Name
mainViewport _state = viewport MainViewport Vertical $ str "Hello World\nasdf\nasdf\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasdfffffffffffffffffffffff\nasdf\nasdf\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasdfffffffffffffffffffffff\nasdf\nasdf\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasdfffffffffffffffffffffff"

statusBar :: State -> Widget Name
statusBar (State{info, lastEvent}) = txt $ "q: " <> info <> lastEventStr lastEvent
  where
    lastEventStr :: Maybe (BrickEvent Name Event) -> Text
    lastEventStr Nothing = ""
    lastEventStr (Just ev) = ", " <> pack (show ev)
