module Q.Home (
  homeDaemon,
) where

import Data.Text
import Q.Mqtt
import Q.Mqtt.Zigbee2Mqtt
import Quasar
import Quasar.Prelude
import Network.MQTT.Client


homeDaemon :: String -> IO ()
homeDaemon mqttUri = do
  traceIO $ "Connecting to " <> mqttUri
  mqtt <- connectMqtt mqttUri statusTopic

  traceIO $ "Connected"
  subscribeIkeaDimmer mqtt "Kitchen switch" (kitchenDimmer mqtt)

  await mqtt

statusTopic :: Topic
statusTopic = "q/home/status"

kitchenHue :: Text
kitchenHue = "0x0017880100c54096"

kitchenDimmer :: Mqtt -> IkeaDimmerCallbacks
kitchenDimmer mqtt =
  ikeaDimmerCallbacks {
    on = setKitchenPreset mqtt Bright,
    onLongPress = setKitchenPreset mqtt Colorful,
    off = setKitchenPreset mqtt Off,
    offLongPress = setKitchenPreset mqtt Mood
  }


data LightPreset = Bright | Colorful | Mood | Off

setKitchenPreset :: Mqtt -> LightPreset -> IO ()
setKitchenPreset mqtt preset = do
  hue preset
  switchTasmota mqtt "sonoff01" (stoveLight preset)
  where
    hue Bright = setHueWhite mqtt kitchenHue
    hue Colorful = setHueRainbow mqtt kitchenHue
    hue Mood = setHueDimOrange mqtt kitchenHue
    hue Off = setHueState mqtt kitchenHue False
    stoveLight Bright = True
    stoveLight Colorful = True
    stoveLight Mood = False
    stoveLight Off = False


switchTasmota :: Mqtt -> Text -> Bool -> IO ()
switchTasmota Mqtt{mqttClient} name value =
  publish mqttClient topic message False
  where
    topic = mconcat ["cmnd/", name, "/POWER"]
    message = if value then "on" else "off"
