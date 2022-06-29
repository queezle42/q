module Q.Home (
  homeDaemon,
) where

import Data.Text
import Data.ByteString.Lazy qualified as BSL
import Q.Mqtt
import Q.Mqtt.Zigbee2Mqtt
import Quasar
import Quasar.Prelude
import Network.MQTT.Client


homeDaemon :: String -> IO ()
data LightPreset = Bright | Colorful | Mood | Off

homeDaemon mqttUri = do
  traceIO $ "Connecting to " <> mqttUri
  mqtt <- connectMqtt mqttUri statusTopic

  traceIO $ "Connected"
  liftIO $ subscribeIkeaDimmer mqtt "Kitchen switch" (kitchenDimmer mqtt)
  liftIO $ subscribeIkeaDimmer mqtt "Bedroom switch" (bedroomDimmer mqtt)
  liftIO $ subscribeIkeaDimmer mqtt "Hallway switch" (hallwayDimmer mqtt)

  await mqtt

statusTopic :: Topic
statusTopic = "q/home/status"

kitchenHue :: Text
kitchenHue = "0x0017880100c54096"

dimmer :: (LightPreset -> IO ()) -> IkeaDimmerCallbacks
dimmer fn =
  ikeaDimmerCallbacks {
    on = fn Colorful,
    onLongPress = fn Bright,
    off = fn Off,
    offLongPress = fn Mood
  }

kitchenDimmer :: Mqtt -> IkeaDimmerCallbacks
kitchenDimmer mqtt =
  ikeaDimmerCallbacks {
    -- Bright and colorful is swapped for the kitchen
    on = setKitchenPreset mqtt Bright,
    onLongPress = setKitchenPreset mqtt Colorful,
    off = setKitchenPreset mqtt Off,
    offLongPress = setKitchenPreset mqtt Mood
  }

bedroomDimmer :: Mqtt -> IkeaDimmerCallbacks
bedroomDimmer mqtt = dimmer (setBedroomPreset mqtt)

hallwayDimmer :: Mqtt -> IkeaDimmerCallbacks
hallwayDimmer mqtt = dimmer (setHallwayPreset mqtt)



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

setBedroomPreset :: Mqtt -> LightPreset -> IO ()
setBedroomPreset mqtt Bright = do
  ringclock mqtt "k8"
  switchTasmota mqtt "sms04" True
  heribert mqtt "warm_white"
setBedroomPreset mqtt Colorful = do
  ringclock mqtt "k8"
  switchTasmota mqtt "sms04" True
  heribert mqtt "peachy"
setBedroomPreset mqtt Mood = do
  heribert mqtt "off"
  switchTasmota mqtt "sms04" False
  ringclock mqtt "k8"
setBedroomPreset mqtt Off = do
  heribert mqtt "off"
  switchTasmota mqtt "sms04" False
  ringclock mqtt "off"

setHallwayPreset :: Mqtt -> LightPreset -> IO ()
setHallwayPreset mqtt Bright = do
  fairyLights mqtt "cozy"
  fairyLightsBrightness mqtt "1"
setHallwayPreset mqtt Colorful = do
  fairyLights mqtt "cozy"
  fairyLightsBrightness mqtt "0.4"
setHallwayPreset mqtt Mood = do
  fairyLights mqtt "cozy"
  fairyLightsBrightness mqtt "0.1"
setHallwayPreset mqtt Off = do
  fairyLightsBrightness mqtt "0"


switchTasmota :: Mqtt -> Text -> Bool -> IO ()
switchTasmota Mqtt{mqttClient} name value =
  publish mqttClient topic message False
  where
    topic = mconcat ["cmnd/", name, "/POWER"]
    message = if value then "on" else "off"

qthingAnimation :: Mqtt -> Text -> BSL.ByteString -> IO ()
qthingAnimation Mqtt{mqttClient} name animation = publish mqttClient (mconcat ["device/", name, "/animation"]) animation False

qthingBrightness :: Mqtt -> Text -> BSL.ByteString -> IO ()
qthingBrightness Mqtt{mqttClient} name value = publish mqttClient (mconcat ["device/", name, "/brightness"]) value False


ringclock :: Mqtt -> BSL.ByteString -> IO ()
ringclock mqtt = qthingAnimation mqtt "ringclock"

heribert :: Mqtt -> BSL.ByteString -> IO ()
heribert mqtt = qthingAnimation mqtt "heribert"

fairyLights :: Mqtt -> BSL.ByteString -> IO ()
fairyLights mqtt = qthingAnimation mqtt "fairy-lights"

fairyLightsBrightness :: Mqtt -> BSL.ByteString -> IO ()
fairyLightsBrightness Mqtt{mqttClient} brightness = publish mqttClient (mconcat ["device/fairy-lights/update/brightness"]) brightness False
