module Q.Home (
  homeDaemon,
) where

import Control.Concurrent.STM
import Data.Text
import Data.ByteString.Lazy qualified as BSL
import Q.Mqtt
import Q.Mqtt.Zigbee2Mqtt
import Quasar
import Quasar.Prelude
import Network.MQTT.Client


homeDaemon :: String -> IO ()
data LightLevel = Bright | Colorful | Mood | Off
  deriving (Eq, Show)

data ButtonRepeat = Initial | Repeat
  deriving (Eq, Show)

data RoomDefinition = RoomDefinition {
  lightBright :: IO (),
  lightColorful :: [IO ()],
  lightMood :: [IO ()],
  lightOff :: IO (),
  preferBright :: Bool
}

roomDefinition :: RoomDefinition
roomDefinition = RoomDefinition {
  lightBright = pure (),
  lightColorful = [pure ()],
  lightMood = [pure ()],
  lightOff = pure (),
  preferBright = False
}

data RoomController = RoomController {
  room :: RoomDefinition,
  currentLevel :: TVar LightLevel,
  currentGenerator :: TVar [IO ()]
}

homeDaemon mqttUri = do
  traceIO $ "Connecting to " <> mqttUri
  mqtt <- connectMqtt mqttUri statusTopic

  traceIO $ "Connected"
  liftIO $ subscribeIkeaDimmer mqtt "switch_kitchen" (kitchenDimmer mqtt)
  liftIO $ subscribeIkeaDimmer mqtt "switch_bedroom_1" (bedroomDimmer mqtt)
  liftIO $ subscribeIkeaDimmer mqtt "switch_bedroom_2" (bedroomDimmer mqtt)

  roomHallway <- liftIO $ newRoomController (hallway mqtt)
  liftIO $ subscribeIkeaDimmer mqtt "switch_hallway" (dimmerHandlerForRoom roomHallway)

  roomLivingRoom <- liftIO $ newRoomController (livingRoom mqtt)
  liftIO $ subscribeIkeaDimmer mqtt "switch_living_room" (dimmerHandlerForRoom roomLivingRoom)

  await mqtt

statusTopic :: Topic
statusTopic = "q/home/status"

kitchenHue :: Text
kitchenHue = "hue_kitchen"

livingRoomHue :: Text
livingRoomHue = "hue_living_room"

newRoomController :: RoomDefinition -> IO RoomController
newRoomController room = do
  -- TODO restore preset when reloading
  let level = Off
  currentLevel <- newTVarIO level
  currentGenerator <- newTVarIO (lightGenerator room level)
  pure RoomController { room, currentLevel, currentGenerator }

lightGenerator :: RoomDefinition -> LightLevel -> [IO ()]
lightGenerator room Bright = [lightBright room]
lightGenerator room Colorful = lightColorful room
lightGenerator room Mood = lightMood room
lightGenerator room Off = [lightOff room]

roomLightLevel :: RoomController -> LightLevel -> IO ()
roomLightLevel RoomController{room, currentLevel, currentGenerator} level = join $ atomically do
  currentLevel' <- readTVar currentLevel
  currentGenerator' <- readTVar currentGenerator
  case (level == currentLevel', currentGenerator') of
    (True, gen:gs) -> do
      writeTVar currentGenerator gs
      pure gen
    _ -> do
      writeTVar currentLevel level
      let (gen:gs) = lightGenerator room level
      writeTVar currentGenerator gs
      pure gen

dimmerHandlerForRoom :: RoomController -> IkeaDimmerCallbacks
dimmerHandlerForRoom roomController =
  ikeaDimmerCallbacks {
    on = roomLightLevel roomController Colorful,
    onLongPress = roomLightLevel roomController Bright,
    off = roomLightLevel roomController Off,
    offLongPress = roomLightLevel roomController Mood
  }

fnDimmer :: (LightLevel -> IO ()) -> IkeaDimmerCallbacks
fnDimmer fn =
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
bedroomDimmer mqtt = fnDimmer (setBedroomPreset mqtt)

hallwayDimmer :: Mqtt -> IkeaDimmerCallbacks
hallwayDimmer mqtt = fnDimmer (setHallwayPreset mqtt)



setKitchenPreset :: Mqtt -> LightLevel -> IO ()
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

setBedroomPreset :: Mqtt -> LightLevel -> IO ()
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

setHallwayPreset :: Mqtt -> LightLevel -> IO ()
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

hallway :: Mqtt -> RoomDefinition
hallway mqtt = roomDefinition { lightBright, lightColorful, lightMood, lightOff }
  where
    lightBright = cozy "1"
    lightColorful = [cozy "0.4", k8 "0.4"]
    lightMood = [cozy "0.1", k8 "0.1"]
    lightOff = fairyLightsBrightness mqtt "0"
    cozy brightness = do
      fairyLights mqtt "cozy"
      fairyLightsBrightness mqtt brightness
    k8 brightness = do
      fairyLights mqtt "k8"
      fairyLightsBrightness mqtt brightness

livingRoom :: Mqtt -> RoomDefinition
livingRoom mqtt = roomDefinition { lightBright, lightColorful, lightMood, lightOff }
  where
    lightBright = setHueWhite mqtt livingRoomHue
    lightColorful = [
      setHueOrange mqtt livingRoomHue,
      setHueRainbow mqtt livingRoomHue
      ]
    lightMood = [ setHueDimOrange mqtt livingRoomHue ]
    lightOff = setHueState mqtt livingRoomHue False

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
