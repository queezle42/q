module Q.Home (
  homeDaemon,
) where

import Q.Mqtt
import Q.Mqtt.Zigbee2Mqtt
import Quasar
import Quasar.Prelude
import Network.MQTT.Topic


homeDaemon :: String -> IO ()
homeDaemon mqttUri = do
  traceIO $ "Connecting to " <> mqttUri
  mqtt <- connectMqtt mqttUri statusTopic

  traceIO $ "Connected"
  subscribeIkeaDimmer mqtt "Kitchen switch" (kitchenDimmer mqtt)

  await mqtt

statusTopic :: Topic
statusTopic = "q/home/status"

kitchenDimmer :: Mqtt -> IkeaDimmerCallbacks
kitchenDimmer mqtt =
  ikeaDimmerCallbacks {
    on = setHueState mqtt "zigbee2mqtt/0x0017880100c54096" True,
    off = setHueState mqtt "zigbee2mqtt/0x0017880100c54096" False
  }
