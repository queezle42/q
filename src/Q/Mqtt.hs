module Q.Mqtt (
  Mqtt,
  connectMqtt,
  subscribeCallback,
  subscribeJson,

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
import Quasar
import Quasar.Async.Unmanaged
import Quasar.Prelude
import Network.MQTT.Client as MQTT
import Network.MQTT.Topic
import Network.URI

data Mqtt = Mqtt {
  client :: MQTTClient,
  callbacks :: TVar [Callback],
  awaitable :: Awaitable ()
}
instance IsAwaitable () Mqtt where
  toAwaitable = awaitable

type CallbackFn = Mqtt -> Topic -> BSL.ByteString -> [Property] -> IO ()
data Callback = Callback {
  topicFilter :: Filter,
  callbackFn :: CallbackFn
}

type JsonCallback = forall a. FromJSON a => Mqtt -> Topic -> a -> [Property] -> IO ()

zigbee2mqtt :: Text -> Topic
zigbee2mqtt name = "zigbee2mqtt/" <> name

connectMqtt :: String -> Topic -> IO Mqtt
connectMqtt mqttUri statusTopic = mfix \handle -> do
  uri <- case parseURI mqttUri of
           Just uri -> pure uri
           Nothing -> fail "Invalid URI"
  client <- connectURI (config handle statusTopic) uri

  callbacks <- newTVarIO []

  publish client statusTopic "online" True

  awaitable <- toAwaitable <$> unmanagedAsync (waitForClient client)

  pure Mqtt {
    client,
    callbacks,
    awaitable
  }

config :: Mqtt -> Topic -> MQTTConfig
config handle statusTopic =
  mqttConfig {
    _lwt = Just $ mkLWT statusTopic "" True,
    _msgCB = SimpleCallback (dispatchCallback handle)
  }

dispatchCallback :: Mqtt -> MQTTClient -> Topic -> BSL.ByteString -> [Property] -> IO ()
dispatchCallback handle@Mqtt{callbacks} client topic content properties = do
  traceIO $ "Received: " <> show topic <> " " <> show content
  cbs <- atomically $ readTVar callbacks
  mapM_ callMatch cbs
  where
    callMatch :: Callback -> IO ()
    callMatch Callback{topicFilter, callbackFn}
      | match topicFilter topic = do
          traceIO $ "Match: " <> show topicFilter
          callbackFn handle topic content properties
      | otherwise = traceIO $ "No match: " <> show topicFilter

subscribeSingle :: MQTTClient -> Filter -> IO ()
subscribeSingle client switchTopic = do
  subs <- MQTT.subscribe client [(switchTopic, subOptions)] []
  case subs of
    ([Right _], _) -> pure ()
    ret -> fail $ "Some error occured while subscribing: " <> show ret


subscribeCallback :: Mqtt -> Filter -> CallbackFn -> IO ()
subscribeCallback Mqtt{client, callbacks} topicFilter fn = do
  atomically $ modifyTVar callbacks (newCallback : )
  subscribeSingle client topicFilter
  where
    newCallback = Callback {
      topicFilter,
      callbackFn = fn
    }

subscribeJson :: forall a. FromJSON a => Mqtt -> Filter -> (Mqtt -> Topic -> a -> [Property] -> IO ()) -> IO ()
subscribeJson handle topicFilter fn = do
  subscribeCallback handle topicFilter decodeCb
  where
    decodeCb :: Mqtt -> Topic -> BSL.ByteString -> [Property] -> IO ()
    decodeCb handle' topic msg props =
      case eitherDecode msg of
        Left err -> traceIO $ mconcat ["Failed to decode json message on topic ", show topic, ": ", err]
        Right json -> fn handle' topic json props


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
setHueState Mqtt{client} hueTopic state = publish client (hueTopic <> "/set") (stateMessage state) False
  where
    stateMessage :: Bool -> BSL.ByteString
    stateMessage False = "{\"state\":\"off\"}"
    stateMessage True = "{\"state\":\"on\"}"
