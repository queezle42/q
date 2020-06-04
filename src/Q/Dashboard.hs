module Q.Dashboard where

import Brick
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text
import qualified Graphics.Vty as Vty

data State = State Text
data Event = Event
data Name = MainViewport
  deriving (Eq, Ord, Show)

run :: IO ()
run = do
  let initialState = State "initialized"
  _finalState <- defaultMain app initialState
  return ()

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
    appHandleEvent _state (VtyEvent (Vty.EvKey (Vty.KChar char) [])) = continue $ State $ singleton char

    -- TODO this should work, but does not right now
    appHandleEvent state (VtyEvent (Vty.EvMouseDown _x _y Vty.BScrollDown [])) = halt state
    appHandleEvent state (VtyEvent (Vty.EvMouseUp _x _y _button)) = halt state

    appHandleEvent state _event = continue state

    appStartEvent :: State -> EventM Name State
    appStartEvent state = do
      vty <- getVtyHandle
      let output = Vty.outputIface vty
      when (Vty.supportsMode output Vty.Mouse) $
        liftIO $ Vty.setMode output Vty.Mouse True
      return state

    appAttrMap :: State -> AttrMap
    appAttrMap _state = attrMap Vty.defAttr []

mainLayout :: State -> Widget Name
mainLayout state = mainViewport state <=> statusBar state

mainViewport :: State -> Widget Name
mainViewport _state = viewport MainViewport Vertical $ str "Hello World\nasdf\nasdf\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasdfffffffffffffffffffffff\nasdf\nasdf\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasdfffffffffffffffffffffff\nasdf\nasdf\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasd\nasdf\nasdfffffffffffffffffffffff"

statusBar :: State -> Widget Name
statusBar (State info) = str $ "q: " <> unpack info

