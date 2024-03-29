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

homeDaemon :: String -> QuasarIO ()
homeDaemon mqttUri = do
  traceIO $ "Connecting to " <> mqttUri
  mqtt <- connectMqtt mqttUri statusTopic

  traceIO $ "Connected"
  liftIO $ subscribeIkeaDimmer mqtt "switch_kitchen" (kitchenDimmer mqtt)
  liftIO $ subscribeIkeaDimmer mqtt "switch_bedroom_1" (bedroomDimmer mqtt)
  liftIO $ subscribeIkeaDimmer mqtt "switch_bedroom_2" (bedroomDimmer mqtt)
  liftIO $ subscribeIkeaDimmer mqtt "switch_bedroom_3" (bedroomDimmer mqtt)

  roomHallway <- liftIO $ newRoomController (hallway mqtt)
  liftIO $ subscribeIkeaDimmer mqtt "switch_hallway" (dimmerHandlerForRoom roomHallway)

  roomLivingRoom <- liftIO $ newRoomController (livingRoom mqtt)
  liftIO $ subscribeIkeaDimmer mqtt "switch_living_room" (dimmerHandlerForRoom roomLivingRoom)

  liftIO $ subscribeMotion mqtt "kitchen_motion_1" (kitchenMotion mqtt)

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

kitchenMotion :: Mqtt -> MotionCallbacks
kitchenMotion mqtt = MotionCallbacks {
  motionDetectedBright = pure (),
  motionDetectedDark = stoveLight mqtt True,
  motionIdle = setKitchenPreset mqtt Off
}

bedroomDimmer :: Mqtt -> IkeaDimmerCallbacks
bedroomDimmer mqtt = fnDimmer (setBedroomPreset mqtt)

hallwayDimmer :: Mqtt -> IkeaDimmerCallbacks
hallwayDimmer mqtt = fnDimmer (setHallwayPreset mqtt)



setKitchenPreset :: Mqtt -> LightLevel -> IO ()
setKitchenPreset mqtt preset = do
  hue preset
  stoveLight mqtt (stoveLightState preset)
  where
    hue Bright = setHueWhite mqtt kitchenHue
    hue Colorful = setHueRainbow mqtt kitchenHue
    hue Mood = setHueState mqtt kitchenHue False
    hue Off = setHueState mqtt kitchenHue False
    stoveLightState Bright = True
    stoveLightState Colorful = True
    stoveLightState Mood = True
    stoveLightState Off = False

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
    lightBright = do
      setHueWhite mqtt livingRoomHue
      triangle01 mqtt "warm_white" "0.5"
      steve mqtt "worklight"
    lightColorful = [
      colorfulA mqtt,
      colorfulB mqtt,
      rainbow mqtt
      ]
      where
        colorfulA mqtt = do
          setHueOrange mqtt livingRoomHue
          triangle01 mqtt "warm" "0.3"
          steve mqtt "off"
        colorfulB mqtt = do
          setHueOrange mqtt livingRoomHue
          triangle01 mqtt "peachy" "0.2"
          steve mqtt "k8"
        rainbow mqtt = do
          setHueRainbow mqtt livingRoomHue
          triangle01 mqtt "rainbow" "0.3"
          steve mqtt "rgb"
    lightMood = [
      do
        setHueDimOrange mqtt livingRoomHue
        triangle01 mqtt "cozy" "0.1"
        steve mqtt "steve"
      ]
    lightOff = do
      setHueState mqtt livingRoomHue False
      triangle01 mqtt "off" "0.3"
      steve mqtt "off"

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

triangle01 :: Mqtt -> BSL.ByteString -> BSL.ByteString -> IO ()
triangle01 mqtt@Mqtt{mqttClient} animation brightness = do
  qthingAnimation mqtt "triangle01" animation
  publish mqttClient (mconcat ["device/triangle01/update/brightness"]) brightness False

steve :: Mqtt -> BSL.ByteString -> IO ()
steve mqtt@Mqtt{mqttClient} animation = qthingAnimation mqtt "kitchen" animation

stoveLight :: Mqtt -> Bool -> IO ()
stoveLight mqtt = setSwitchState mqtt "stove_light"
