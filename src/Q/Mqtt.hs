module Q.Mqtt (
  Mqtt(Mqtt, mqttClient),
  connectMqtt,
  subscribeCallback,
  subscribeJson,
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
  mqttClient :: MQTTClient,
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


connectMqtt :: String -> Topic -> IO Mqtt
connectMqtt mqttUri statusTopic = mfix \handle -> do
  uri <- case parseURI mqttUri of
           Just uri -> pure uri
           Nothing -> fail "Invalid URI"
  mqttClient <- connectURI (config handle statusTopic) uri

  callbacks <- newTVarIO []

  publish mqttClient statusTopic "online" True

  awaitable <- toAwaitable <$> unmanagedAsync (waitForClient mqttClient)

  pure Mqtt {
    mqttClient,
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
subscribeCallback Mqtt{mqttClient, callbacks} topicFilter fn = do
  atomically $ modifyTVar callbacks (newCallback : )
  subscribeSingle mqttClient topicFilter
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
