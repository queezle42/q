module Q.Mqtt.Zigbee2Mqtt (
  IkeaDimmerCallbacks(..),
  ikeaDimmerCallbacks,
  subscribeIkeaDimmer,

  setHueState,
) where

import Control.Concurrent.STM
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HM
import Data.Text
import Q.Mqtt
import Quasar
import Quasar.Async.Unmanaged
import Quasar.Prelude
import Network.MQTT.Client as MQTT
import Network.MQTT.Topic
import Network.URI

zigbee2mqtt :: Text -> Topic
zigbee2mqtt name = "zigbee2mqtt/" <> name


data IkeaDimmerCallbacks = IkeaDimmerCallbacks {
  on :: IO (),
  off :: IO (),
  onLongPress :: IO (),
  offLongPress :: IO (),
  endLongPress :: IO ()
}

ikeaDimmerCallbacks :: IkeaDimmerCallbacks
ikeaDimmerCallbacks =
  IkeaDimmerCallbacks {
    on = pure (),
    off = pure (),
    onLongPress = pure (),
    offLongPress = pure (),
    endLongPress = pure ()
  }

subscribeIkeaDimmer :: Mqtt -> Text -> IkeaDimmerCallbacks -> IO ()
subscribeIkeaDimmer handle switchName callbacks = do
  subscribeJson handle (zigbee2mqtt switchName) cb
  where
    cb :: Mqtt -> Topic -> Object -> [Property] -> IO ()
    cb _ _ event _ =
      case HM.lookup "action" event of
        Just (String "on") -> on callbacks
        Just (String "off") -> off callbacks
        Just (String "brightness_move_up") -> onLongPress callbacks
        Just (String "brightness_move_down") -> offLongPress callbacks
        Just (String "brightness_stop") -> endLongPress callbacks
        Just (String action) -> traceIO $ "Unknown switch .action: " <> show action
        Just action -> traceIO $ "Switch event .action should be a string but is " <> show action
        Nothing -> traceIO "Switch event has no .action key"

setHueState :: Mqtt -> Topic -> Bool -> IO ()
setHueState Mqtt{mqttClient} hueTopic state = publish mqttClient (hueTopic <> "/set") (stateMessage state) False
  where
    stateMessage :: Bool -> BSL.ByteString
    stateMessage False = "{\"state\":\"off\"}"
    stateMessage True = "{\"state\":\"on\"}"
