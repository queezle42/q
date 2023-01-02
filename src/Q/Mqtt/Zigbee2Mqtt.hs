module Q.Mqtt.Zigbee2Mqtt (
  IkeaDimmerCallbacks(..),
  ikeaDimmerCallbacks,
  subscribeIkeaDimmer,

  setHueState,
  setHueWhite,
  setHueRainbow,
  setHueOrange,
  setHueDimOrange,

  setSwitchState,
) where

import Data.Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HM
import Data.Text
import Q.Mqtt
import Quasar.Prelude
import Network.MQTT.Client as MQTT

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
        Just (String action) -> traceIO $ "Unknown switch action: " <> show action
        Just action -> traceIO $ "Switch event action should be a string but is " <> show action
        Nothing -> pure () -- Switch event has no .action key, e.g. when availability is announced

setHueState :: Mqtt -> Topic -> Bool -> IO ()
setHueState mqtt hueTopic state = publishSetMessage mqtt hueTopic (stateMessage state)
  where
    stateMessage :: Bool -> BSL.ByteString
    stateMessage False = "{\"state\":\"off\"}"
    stateMessage True = "{\"state\":\"on\"}"

setHueWhite :: Mqtt -> Topic -> IO ()
setHueWhite mqtt hueTopic = publishSetMessage mqtt hueTopic msg
  where
    msg = "{\"color\":{\"h\":50,\"s\":25},\"brightness\":255,\"transition\":1}"

setHueRainbow :: Mqtt -> Topic -> IO ()
setHueRainbow mqtt hueTopic = publishSetMessage mqtt hueTopic msg
  where
    msg = "{\"color\":{\"h\":0,\"s\":85},\"brightness\":255,\"hue_move\":2,\"transition\":1}"

setHueOrange :: Mqtt -> Topic -> IO ()
setHueOrange mqtt hueTopic = publishSetMessage mqtt hueTopic msg
  where
    msg = "{\"color\":{\"h\":40,\"s\":80},\"brightness\":255,\"transition\":1}"

setHueDimOrange :: Mqtt -> Topic -> IO ()
setHueDimOrange mqtt hueTopic = publishSetMessage mqtt hueTopic msg
  where
    msg = "{\"color\":{\"h\":40,\"s\":100},\"brightness\":85,\"transition\":1}"

publishSetMessage :: Mqtt -> Topic -> BSL.ByteString -> IO ()
publishSetMessage Mqtt{mqttClient} deviceName msg = publish mqttClient (zigbee2mqtt deviceName <> "/set") msg False


setSwitchState :: Mqtt -> Topic -> Bool -> IO ()
setSwitchState mqtt deviceName state = publishSetMessage mqtt deviceName (stateMessage state)
  where
    stateMessage :: Bool -> BSL.ByteString
    stateMessage False = "{\"state\":\"off\"}"
    stateMessage True = "{\"state\":\"on\"}"
