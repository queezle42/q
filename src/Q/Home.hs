module Q.Home (
  homeDaemon,
) where

import Control.Concurrent.STM
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HM
import Data.Text
import Q.Mqtt
import Q.Mqtt.Zigbee2Mqtt
import Quasar
import Quasar.Prelude
import Network.MQTT.Client
import Network.MQTT.Topic
import Network.URI


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
