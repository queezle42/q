module Q.Mqtt (
  Mqtt(Mqtt, mqttClient),
  connectMqtt,
  subscribeCallback,
  subscribeJson,

  -- * Reexports from Network.MQTT
  Topic,
  Filter,
) where

import Data.Aeson
import Data.ByteString.Lazy qualified as BSL
import Quasar
import Quasar.Prelude
import Network.MQTT.Client as MQTT
import Network.MQTT.Topic
import Network.URI

data Mqtt = Mqtt {
  mqttClient :: MQTTClient,
  callbacks :: TVar [Callback],
  disconnectedFuture :: Future ()
}

instance IsFuture () Mqtt where
  toFuture = disconnectedFuture

type CallbackFn = Mqtt -> Topic -> BSL.ByteString -> [Property] -> IO ()
data Callback = Callback {
  topicFilter :: Filter,
  callbackFn :: CallbackFn
}


connectMqtt :: String -> Topic -> QuasarIO Mqtt
connectMqtt mqttUri statusTopic = mfix \handle -> do
  uri <- case parseURI mqttUri of
           Just uri -> pure uri
           Nothing -> fail "Invalid URI"
  mqttClient <- liftIO $ connectURI (config handle statusTopic) uri

  callbacks <- newTVarIO []

  liftIO $ publish mqttClient statusTopic "online" True

  disconnectedFuture <- toFuture <$> async (liftIO $ waitForClient mqttClient)

  pure Mqtt {
    mqttClient,
    callbacks,
    disconnectedFuture
  }

config :: Mqtt -> Topic -> MQTTConfig
config handle statusTopic =
  mqttConfig {
    _lwt = Just $ mkLWT statusTopic "" True,
    _msgCB = SimpleCallback (dispatchCallback handle)
  }

dispatchCallback :: Mqtt -> MQTTClient -> Topic -> BSL.ByteString -> [Property] -> IO ()
dispatchCallback handle@Mqtt{callbacks} _ topic content properties = do
  cbs <- readTVarIO callbacks
  mapM_ callMatch cbs
  where
    callMatch :: Callback -> IO ()
    callMatch Callback{topicFilter, callbackFn} =
      when (match topicFilter topic) do
        callbackFn handle topic content properties

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
        Right jsonMsg -> fn handle' topic jsonMsg props
